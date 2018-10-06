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

-- specjvm_runs

/** updateJobsSeq */
update specjvm_jobs_seq
set value = value + 1

/** selectNewJobId */
select value as id
from specjvm_jobs_seq

/** insertJob */
insert into specjvm_jobs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateJobState */
update specjvm_jobs
set   state = :state
where id = :id

/** updateJobFinish */
update specjvm_jobs
set   state = :state
    , finish_date = :finishDate
    , total_time_seconds = :totalTimeSeconds
where id = :id

-- specjvm_results

/** updateResultsSeq */
update specjvm_results_seq
set value = value + 1

/** selectNewResultId */
select value as id
from specjvm_results_seq

/** insertResult */
insert into specjvm_results (id, name, mode, counts, score, error, units, job_id)
values (:id, :name, :mode, :counts, :score, :error, :units, :jobId)

/** selectResultsByTaskId */
select
      sr.name as name
    , sr.mode as mode
    , sr.counts as counts
    , sr.score as score
    , sr.error as error
    , sr.units as units
from specjvm_results as sr
join specjvm_jobs as sj
    on sr.job_id = sj.id
where sj.task_id = :taskId
