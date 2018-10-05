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

create table jdkbuild_jobs_seq (value bigint);
insert into jdkbuild_jobs_seq (value) values (0);
create table jdkbuild_jobs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , repository text
    , revision text
    , task_id int not null
    , foreign key (task_id) references tasks (id)
    );
create index jdkbuild_jobs__start_date_idx on jdkbuild_jobs (start_date);


-- tier1

-- jobs
create table tier1_jobs_seq (value bigint);
insert into tier1_jobs_seq (value) values (0);
create table tier1_jobs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , total_not_pass int
    , task_id int not null
    , foreign key (task_id) references tasks (id)
    );
create index tier1_jobs__start_date_idx on tier1_jobs(start_date);

-- test results
create table tier1_results_seq (value bigint);
insert into tier1_results_seq (value) values (0);
create table tier1_results
    ( id bigint primary key
    , name text not null
    , pass int not null
    , fail int not null
    , error int not null
    , job_id int not null
    , foreign key (job_id) references tier1_jobs (id)
    );

-- jcstress

-- jobs
create table jcstress_jobs_seq (value bigint);
insert into jcstress_jobs_seq (value) values (0);
create table jcstress_jobs
    ( id bigint primary key
    , start_date date time not null
    , finish_date date time
    , state text not null
    , total_fail_or_error int
    , task_id int not null
    , foreign key (task_id) references tasks (id)
    );
create index jcstress_jobs__start_date_idx on jcstress_jobs(start_date);

-- results
create table jcstress_results_seq (value bigint);
insert into jcstress_results_seq (value) values (0);
create table jcstress_results
    ( id bigint primary key
    , passed int
    , interesting int
    , failed int
    , error int
    , job_id int not null
    , foreign key (job_id) references jcstress_jobs (id)
    );

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
    , task_id int not null
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
    , run_id int not null
    , foreign key (run_id) references specjvm_runs (id)
    );

commit;
