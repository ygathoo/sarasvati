-- DROP EXISTING TABLES
drop table if exists wf_token_attr cascade;
drop table if exists wf_process_attr cascade;
drop table if exists wf_node_token_parent cascade;
drop table if exists wf_arc_token cascade;
drop table if exists wf_node_token cascade;
drop table if exists wf_execution_type cascade;
drop table if exists wf_arc cascade;
drop table if exists wf_node_ref cascade;
drop table if exists wf_node_attr cascade;
drop table if exists wf_node cascade;
drop table if exists wf_node_type cascade;
drop table if exists wf_node_join_type cascade;
drop table if exists wf_guard_action cascade;
drop table if exists wf_process_listener cascade;
drop table if exists wf_process cascade;
drop table if exists wf_process_state cascade;
drop table if exists wf_graph cascade;

-- CREATE NEW TABLES

create table wf_graph
(
  id          serial    NOT NULL PRIMARY KEY,
  name        text      NOT NULL,
  version     int       NOT NULL,
  create_date timestamp NOT NULL DEFAULT current_timestamp
);

ALTER TABLE wf_graph
  ADD CONSTRAINT wf_graph_unique
    UNIQUE(name,version);

create table wf_process_state
(
  id int NOT NULL PRIMARY KEY,
  description text NOT NULL
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
  graph_id        int          NOT NULL REFERENCES wf_graph,
  state           int          NOT NULL REFERENCES wf_process_state,
  parent_token_id int          NULL,
  create_date     timestamp    NOT NULL DEFAULT current_timestamp,
  version         int          NOT NULL
);

create table wf_process_attr
(
  process_id  int  NOT NULL REFERENCES wf_process,
  name        text NOT NULL,
  value       text NOT NULL
);

ALTER TABLE wf_process_attr
  ADD PRIMARY KEY (process_id, name);

create table wf_process_listener
(
  id              serial NOT NULL PRIMARY KEY,
  type            text   NOT NULL,
  event_type      int    NOT NULL,
  process_id      int    NOT NULL REFERENCES wf_process
);

ALTER TABLE wf_process_listener
  ADD CONSTRAINT wf_listener_unique
    UNIQUE(type, event_type, process_id);

create table wf_node_type
(
  id          text NOT NULL PRIMARY KEY,
  description text NOT NULL,
  behaviour   text NOT NULL REFERENCES wf_node_type
);

insert into wf_node_type values ( 'node', 'Generic node allowing for many inputs, many outputs and guards', 'node' );
insert into wf_node_type values ( 'custom', 'Supertype for user custom node types', 'custom' );
insert into wf_node_type values ( 'wait', 'Node which enters a wait state when executed', 'custom' );
insert into wf_node_type values ( 'script', 'Node which executes a script', 'custom' );
insert into wf_node_type values ( 'nested', 'Node which executes a nested process', 'custom' );

create table wf_node_join_type
(
  id int NOT NULL PRIMARY KEY,
  description text NOT NULL
);

insert into wf_node_join_type values ( 0, 'Or: Join is completed whenever any arc token arrives at the node' );
insert into wf_node_join_type values ( 1, 'And: Join is completed when there are arc tokens on all incoming arcs to a node' );
insert into wf_node_join_type values ( 2, 'Label And: Join is completed when there are arc tokens on all incoming arcs with the same name as that of the arc that the current incoming arc token is on.' );

create table wf_node
(
  id              serial  NOT NULL PRIMARY KEY,
  graph_id        int     NOT NULL REFERENCES wf_graph,
  name            text    NOT NULL,
  join_type       int     NOT NULL REFERENCES wf_node_join_type,
  is_start        char(1) NOT NULL,
  type            text    NOT NULL REFERENCES wf_node_type,
  guard           text    NULL
);

ALTER TABLE wf_node
  ADD CONSTRAINT wf_node_unique
    UNIQUE(graph_id, name);

create table wf_node_attr
(
  node_id  int  NOT NULL REFERENCES wf_node,
  name     text NOT NULL,
  value    text NOT NULL
);

ALTER TABLE wf_node_attr
  ADD PRIMARY KEY (node_id, name);

create table wf_node_ref
(
  id        serial NOT NULL PRIMARY KEY,
  node_id   int    NOT NULL REFERENCES wf_node,
  graph_id  int    NOT NULL REFERENCES wf_graph,
  instance  text   NOT NULL
);

create table wf_arc
(
  id            serial NOT NULL PRIMARY KEY,
  graph_id      int    NOT NULL REFERENCES wf_graph,
  a_node_ref_id int    NOT NULL REFERENCES wf_node_ref,
  z_node_ref_id int    NOT NULL REFERENCES wf_node_ref,
  name          text   NULL
);

create table wf_guard_action
(
  id           int  NOT NULL PRIMARY KEY,
  name         text NOT NULL
);

insert into wf_guard_action values ( 0, 'Accept Token' );
insert into wf_guard_action values ( 1, 'Discard Token' );
insert into wf_guard_action values ( 2, 'Skip Node' );

create table wf_execution_type
(
  id            int  NOT NULL PRIMARY KEY,
  name          text NOT NULL
);

insert into wf_execution_type values ( 0, 'Forward' );
insert into wf_execution_type values ( 1, 'Forward/Backtracked' );
insert into wf_execution_type values ( 2, 'Backtracked' );
insert into wf_execution_type values ( 3, 'U-Turn' );
insert into wf_execution_type values ( 4, 'U-Turn/Backtracked' );

create table wf_node_token
(
  id              serial    NOT NULL PRIMARY KEY,
  process_id      int       NOT NULL REFERENCES wf_process,
  node_ref_id     int       NOT NULL REFERENCES wf_node_ref,
  attr_set_id     int       NULL     REFERENCES wf_node_token,
  create_date     timestamp NOT NULL DEFAULT current_timestamp,
  guard_action    int       NULL     REFERENCES wf_guard_action,
  execution_type  int       NOT NULL REFERENCES wf_execution_type,
  complete_date   timestamp NULL

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
  pending         char(1)   NOT NULL,
  execution_type  int       NOT NULL REFERENCES wf_execution_type,
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
  attr_set_id  int  NOT NULL REFERENCES wf_node_token,
  name         text NOT NULL,
  value        text NOT NULL
);

ALTER TABLE wf_token_attr
  ADD PRIMARY KEY (attr_set_id, name);
