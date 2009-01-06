-- Fix circular reference
alter table wf_process drop constraint FK_process_parent
go

-- DROP EXISTING TABLES
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_task') drop table wf_task
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_task_state') drop table wf_task_state
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_task') drop table wf_node_task
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_token_attr') drop table wf_token_attr
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_token_parent') drop table wf_node_token_parent
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_arc_token') drop table wf_arc_token
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_token') drop table wf_node_token
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_execution_type') drop table wf_execution_type
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_arc') drop table wf_arc
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_ref') drop table wf_node_ref
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_attr') drop table wf_node_attr
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_script') drop table wf_node_script
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node') drop table wf_node
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_node_type') drop table wf_node_type
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_guard_action') drop table wf_guard_action
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_process_attr') drop table wf_process_attr
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_process_listener') drop table wf_process_listener
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_process') drop table wf_process
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_process_state') drop table wf_process_state
IF EXISTS (SELECT * FROM sysobjects WHERE name='wf_graph') drop table wf_graph
go

-- CREATE NEW TABLES

create table wf_graph
(
  id          bigint       IDENTITY NOT NULL PRIMARY KEY,
  name        varchar(255)          NOT NULL,
  version     int                   NOT NULL,
  create_date datetime              DEFAULT getDate() NOT NULL
) with identity_gap = 10
go


ALTER TABLE wf_graph
  ADD CONSTRAINT wf_graph_unique
    UNIQUE(name,version)
go

create table wf_process_state
(
  id          int          NOT NULL PRIMARY KEY,
  description varchar(255) NOT NULL
)
go

insert into wf_process_state values ( 0, 'Created' )
insert into wf_process_state values ( 1, 'Executing' )
insert into wf_process_state values ( 2, 'Pending Completion' )
insert into wf_process_state values ( 3, 'Completed' )
insert into wf_process_state values ( 4, 'Pending Cancel' )
insert into wf_process_state values ( 5, 'Canceled' )
go

create table wf_process
(
  id              bigint       IDENTITY NOT NULL PRIMARY KEY,
  graph_id        bigint                NOT NULL,
  state           int                   NOT NULL REFERENCES wf_process_state,
  parent_token_id bigint                NULL,
  create_date     datetime              DEFAULT getDate() NOT NULL,
  version         int                   NOT NULL
) with identity_gap = 10
go

create table wf_process_attr
(
  process_id   bigint       NOT NULL REFERENCES wf_process,
  name         varchar(64)  NOT NULL,
  value        varchar(255) NOT NULL
)
go

ALTER TABLE wf_process_attr
  ADD PRIMARY KEY (process_id, name)
go

create table wf_process_listener
(
  id              bigint       IDENTITY NOT NULL PRIMARY KEY,
  type            varchar(255)          NOT NULL,
  event_type      int                   NOT NULL,
  process_id      bigint                NOT NULL REFERENCES wf_process
) with identity_gap = 10
go

ALTER TABLE wf_process_listener
  ADD CONSTRAINT wf_listener_unique
    UNIQUE(type, event_type, process_id)
go

create table wf_node_type
(
   id          varchar(255) NOT NULL PRIMARY KEY,
   description varchar(255) NOT NULL,
   behaviour   varchar(255) NOT NULL REFERENCES wf_node_type
)
go

insert into wf_node_type values ( 'node', 'Generic node allowing for many inputs, many outputs and guards', 'node' )
insert into wf_node_type values ( 'wait', 'Node which enters a wait state when executed', 'wait' )
insert into wf_node_type values ( 'script', 'Node which executes a script', 'script' )
insert into wf_node_type values ( 'nested', 'Node which executes a nested process', 'nested' )
insert into wf_node_type values ( 'custom', 'Supertype for user custom node types', 'custom' )
go

create table wf_node
(
  id              bigint       IDENTITY NOT NULL PRIMARY KEY,
  graph_id        bigint                NOT NULL REFERENCES wf_graph,
  name            varchar(255)          NOT NULL,
  is_join         char(1)               NOT NULL,
  is_start        char(1)               NOT NULL,
  type            varchar(255)          NOT NULL REFERENCES wf_node_type,
  guard           varchar(255)          NULL
) with identity_gap = 10
go

ALTER TABLE wf_node
  ADD CONSTRAINT wf_node_unique
    UNIQUE(graph_id, name)
go

create table wf_node_attr
(
  node_id  bigint       NOT NULL REFERENCES wf_node,
  name     varchar(255) NOT NULL,
  value    varchar(255) NOT NULL
)
go

ALTER TABLE wf_node_attr
  ADD PRIMARY KEY (node_id, name)
go

create table wf_node_script
(
  id          bigint       NOT NULL PRIMARY KEY REFERENCES wf_node,
  script      varchar(512) NOT NULL,
  script_type varchar(20)  NOT NULL
)
go

create table wf_node_ref
(
  id        bigint       IDENTITY NOT NULL PRIMARY KEY,
  node_id   bigint                NOT NULL REFERENCES wf_node,
  graph_id  bigint                NOT NULL REFERENCES wf_graph,
  instance  varchar(255)          NOT NULL
) with identity_gap = 10
go

create table wf_arc
(
  id            bigint       IDENTITY NOT NULL PRIMARY KEY,
  graph_id      bigint                NOT NULL REFERENCES wf_graph,
  a_node_ref_id bigint                NOT NULL REFERENCES wf_node_ref,
  z_node_ref_id bigint                NOT NULL REFERENCES wf_node_ref,
  name          varchar(255)          NULL
) with identity_gap = 10
go

create table wf_guard_action
(
  id           int         NOT NULL PRIMARY KEY,
  name         varchar(50) NOT NULL
)
go

insert into wf_guard_action values ( 0, 'Accept Token' )
insert into wf_guard_action values ( 1, 'Discard Token' )
insert into wf_guard_action values ( 2, 'Skip Node' )
go

create table wf_execution_type
(
  id            int          NOT NULL PRIMARY KEY,
  name          varchar(255) NOT NULL
)
go

insert into wf_execution_type values ( 0, 'Forward' )
insert into wf_execution_type values ( 1, 'Forward/Backtracked' )
insert into wf_execution_type values ( 2, 'Backtracked' )
insert into wf_execution_type values ( 3, 'U-Turn' )
insert into wf_execution_type values ( 4, 'U-Turn/Backtracked' )
go

create table wf_node_token
(
  id             bigint    IDENTITY NOT NULL PRIMARY KEY,
  process_id     bigint             NOT NULL REFERENCES wf_process,
  node_ref_id    bigint             NOT NULL REFERENCES wf_node_ref,
  attr_set_id    bigint             NULL     REFERENCES wf_node_token,
  create_date    datetime           DEFAULT getDate() NOT NULL,
  guard_action   int                NULL     REFERENCES wf_guard_action,
  execution_type int                NOT NULL REFERENCES wf_execution_type,
  complete_date  datetime           NULL
) with identity_gap = 100
go

ALTER TABLE wf_process
  ADD CONSTRAINT FK_process_parent
    FOREIGN KEY (parent_token_id)
      REFERENCES wf_node_token
go

create table wf_arc_token
(
  id              bigint    IDENTITY NOT NULL PRIMARY KEY,
  process_id      bigint             NOT NULL REFERENCES wf_process,
  arc_id          bigint             NOT NULL REFERENCES wf_arc,
  parent_token_id bigint             NOT NULL REFERENCES wf_node_token,
  pending         char(1)            NOT NULL,
  execution_type  int                NOT NULL REFERENCES wf_execution_type,
  create_date     datetime           DEFAULT getDate() NOT NULL,
  complete_date   datetime           NULL
) with identity_gap = 100
go

create table wf_node_token_parent
(
   node_token_id bigint NOT NULL REFERENCES wf_node_token,
   arc_token_id  bigint NOT NULL REFERENCES wf_arc_token
)
go

create table wf_token_attr
(
  attr_set_id  bigint       NOT NULL REFERENCES wf_node_token,
  name         varchar(64)  NOT NULL,
  value        varchar(255) NOT NULL
)
go

ALTER TABLE wf_token_attr
  ADD PRIMARY KEY (attr_set_id, name)
go