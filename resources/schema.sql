--
-- Copyright 2018, akashche at redhat.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

begin;

-- top-level tasks

create table tasks_seq (value bigint);
insert into tasks_seq (value) values (0);
create table tasks
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , comment text not null
    );
create index tasks__start_date_idx on tasks (start_date);


-- jdk builds

create table jdkbuild_runs_seq (value bigint);
insert into jdkbuild_runs_seq (value) values (0);
create table jdkbuild_runs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , repository text
    , revision text
    , task_id text not null
    , foreign key (task_id) references tasks (id)
    );
create index jdkbuild_runs__start_date_idx on jdkbuild_runs (start_date);


-- tier1 tests

create table tier1_runs_seq (value bigint);
insert into tier1_runs_seq (value) values (0);
create table tier1_runs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , task_id text not null
    , foreign key (task_id) references tasks (id)
    );
create index tier1_runs__start_date_idx on tier1_runs(start_date);


-- jcstress

create table jcstress_runs_seq (value bigint);
insert into jcstress_runs_seq (value) values (0);
create table jcstress_runs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , passed int
    , passedDiff int
    , interesting int
    , interestingDiff int
    , failed int
    , failedDiff int
    , error int
    , errorDiff int
    , task_id text not null
    , foreign key (task_id) references tasks (id)
    );
create index jcstress_runs__start_date_idx on jcstress_runs(start_date);


-- specjvm

-- runs
create table specjvm_runs_seq (value bigint);
insert into specjvm_runs_seq (value) values (0);
create table specjvm_runs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , total_time_seconds int
    , relative_total_time int
    , task_id text not null
    , foreign key (task_id) references tasks (id)
    );
create index specjvm_runs__start_date_idx on specjvm_runs (start_date);

-- benchmark results
create table specjvm_results_seq (value bigint);
insert into specjvm_results_seq (value) values (0);
create table specjvm_results
    ( id bigint primary key
    , name text not null
    , mode text not null
    , counts int not null
    , score int not null
    , error int not null
    , units text not null
    , run_id text not null
    , foreign key (run_id) references specjvm_runs (id)
    );

-- benchmark diffs
create table specjvm_diffs_seq (value bigint);
insert into specjvm_diffs_seq (value) values (0);
create table specjvm_diffs
    ( id bigint primary key
    , name text not null
    , relative_score int not null
    , run_id text not null
    , foreign key (run_id) references specjvm_runs (id)
    );

commit;
