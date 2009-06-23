drop table if exists wf_task;
drop table if exists wf_task_state;
drop table if exists wf_node_task;

create table wf_node_task
(
  id          int          NOT NULL PRIMARY KEY REFERENCES wf_node,
  name        varchar(127) NOT NULL,
  description varchar(255) NOT NULL
)
go

create table wf_task_state
(
   id          int         NOT NULL  PRIMARY KEY,
   description varchar(64) NOT NULL
)
go

insert into wf_task_state values ( 0, 'Open' )
insert into wf_task_state values ( 1, 'Complete' )
insert into wf_task_state values ( 2, 'Rejected' )
insert into wf_task_state values ( 3, 'Canceled' )
go

create table wf_task
(
  id            serial       NOT NULL PRIMARY KEY,
  node_token_id int          NOT NULL REFERENCES wf_node_token,
  name          varchar(127) NOT NULL,
  description   varchar(255) NOT NULL,
  state         int          NOT NULL REFERENCES wf_task_state
)
go