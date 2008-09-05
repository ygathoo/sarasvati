-- DROP EXISTING TABLES
drop table if exists wf_token_attr cascade;
drop table if exists wf_process_attr cascade;
drop table if exists wf_node_token_parent cascade;
drop table if exists wf_arc_token cascade;
drop table if exists wf_node_token cascade;
drop table if exists wf_arc cascade;
drop table if exists wf_node_ref cascade;
drop table if exists wf_node cascade;
drop table if exists wf_node_type cascade;
drop table if exists wf_guard_action cascade;
drop table if exists wf_process_listener cascade;
drop table if exists wf_process cascade;
drop table if exists wf_process_state cascade;
drop table if exists wf_graph cascade;

-- CREATE NEW TABLES

create table wf_graph
(
  id          serial       NOT NULL PRIMARY KEY,
  name        varchar(255) NOT NULL,
  version     int          NOT NULL,
  create_date timestamp    NOT NULL DEFAULT current_timestamp
);

ALTER TABLE wf_graph
  ADD CONSTRAINT wf_graph_unique
    UNIQUE(name,version);

create table wf_process_state
(
  id int NOT NULL PRIMARY KEY,
  description varchar(255) NOT NULL
);

insert into wf_process_state values ( 0, 'Created' );
insert into wf_process_state values ( 1, 'Executing' );
insert into wf_process_state values ( 2, 'Pending Completion' );
insert into wf_process_state values ( 3, 'Completed' );
insert into wf_process_state values ( 4, 'Pending Cancel' );
insert into wf_process_state values ( 5, 'Canceled' );

create table wf_process
(
  id              serial       NOT NULL PRIMARY KEY,
  graph_id        int          NOT NULL,
  state           int          NOT NULL REFERENCES wf_process_state,
  parent_token_id int          NULL,
  create_date     timestamp    NOT NULL DEFAULT current_timestamp
);

create table wf_process_attr
(
  process_id  int          NOT NULL REFERENCES wf_process,
  name        varchar(64)  NOT NULL,
  value       varchar(255) NOT NULL
);

ALTER TABLE wf_process_attr
  ADD PRIMARY KEY (process_id, name);

create table wf_process_listener
(
  id              serial       NOT NULL PRIMARY KEY,
  type            varchar(255) NOT NULL,
  event_type      int          NOT NULL,
  process_id      int          NOT NULL REFERENCES wf_process
);

ALTER TABLE wf_process_listener
  ADD CONSTRAINT wf_listener_unique
    UNIQUE(type, event_type, process_id);

create table wf_node_type
(
  id          varchar(255) NOT NULL PRIMARY KEY,
  description varchar(255) NOT NULL,
  behaviour   varchar(255) NOT NULL REFERENCES wf_node_type
);

insert into wf_node_type values ( 'node', 'Generic node allowing for many inputs, many outputs and guards', 'node' );
insert into wf_node_type values ( 'wait', 'Node which enters a wait state when executed', 'wait' );
insert into wf_node_type values ( 'task', 'Node which generates tasks', 'task' );
insert into wf_node_type values ( 'init', 'Node which generates a random number and updates a counter', 'init' );
insert into wf_node_type values ( 'dump', 'Node which indicates on stdout that it has been invoked', 'dump' );

create table wf_node
(
  id              serial       NOT NULL PRIMARY KEY,
  graph_id        int          NOT NULL REFERENCES wf_graph,
  name            varchar(255) NOT NULL,
  is_join         char(1)      NOT NULL,
  is_start        char(1)      NOT NULL,
  type            varchar(255) NOT NULL REFERENCES wf_node_type,
  guard           varchar(255) NULL
);

ALTER TABLE wf_node
  ADD CONSTRAINT wf_node_unique
    UNIQUE(graph_id, name);

create table wf_node_ref
(
  id        serial       NOT NULL PRIMARY KEY,
  node_id   int          NOT NULL REFERENCES wf_node,
  graph_id  int          NOT NULL REFERENCES wf_graph,
  instance  varchar(255) NOT NULL
);

create table wf_arc
(
  id            serial       NOT NULL PRIMARY KEY,
  graph_id      int          NOT NULL REFERENCES wf_graph,
  a_node_ref_id int          NOT NULL REFERENCES wf_node_ref,
  z_node_ref_id int          NOT NULL REFERENCES wf_node_ref,
  name          varchar(255) NULL
);

create table wf_guard_action
(
  id           int         NOT NULL PRIMARY KEY,
  name         varchar(50) NOT NULL
);

insert into wf_guard_action values ( 0, 'Accept Token' );
insert into wf_guard_action values ( 1, 'Discard Token' );
insert into wf_guard_action values ( 2, 'Skip Node' );

create table wf_node_token
(
  id            serial    NOT NULL PRIMARY KEY,
  process_id    int       NOT NULL REFERENCES wf_process,
  node_ref_id   int       NOT NULL REFERENCES wf_node_ref,
  attr_set_id   int       NULL     REFERENCES wf_node_token,
  create_date   timestamp NOT NULL DEFAULT current_timestamp,
  guard_action  int       NULL     REFERENCES wf_guard_action,
  complete_date timestamp NULL
);

ALTER TABLE wf_process
  ADD CONSTRAINT FK_process_parent
    FOREIGN KEY (parent_token_id)
      REFERENCES wf_node_token;

create table wf_arc_token
(
  id              serial    NOT NULL PRIMARY KEY,
  process_id      int       NOT NULL REFERENCES wf_process,
  arc_id          int       NOT NULL REFERENCES wf_arc,
  parent_token_id int       NOT NULL REFERENCES wf_node_token,
  executed        char(1)   NOT NULL,
  create_date     timestamp NOT NULL DEFAULT current_timestamp,
  complete_date   timestamp NULL
);

create table wf_node_token_parent
(
   node_token_id INT NOT NULL REFERENCES wf_node_token,
   arc_token_id  INT NOT NULL REFERENCES wf_arc_token
);

create table wf_token_attr
(
  attr_set_id  int          NOT NULL REFERENCES wf_node_token,
  name         varchar(64)  NOT NULL,
  value        varchar(255) NOT NULL
);

ALTER TABLE wf_token_attr
  ADD PRIMARY KEY (attr_set_id, name);
