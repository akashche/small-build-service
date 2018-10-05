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

-- jcstress_jobs

/** updateJobsSeq */
update jcstress_jobs_seq
set value = value + 1

/** selectNewJobId */
select value as id
from jcstress_jobs_seq

/** insertJob */
insert into jcstress_jobs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateJobState */
update jcstress_jobs
set   state = :state
where id = :id

/** updateJobFinish */
update jcstress_jobs
set   state = :state
    , finish_date = :finishDate
    , total_fail_or_error = :totalFailOrError
where id = :id

-- jcstress_results

/** updateResultsSeq */
update jcstress_results_seq
set value = value + 1

/** selectNewResultId */
select value as id
from jcstress_results_seq

/** insertResult */
insert into jcstress_results (id, passed, interesting, failed, error, job_id)
values (:id, :passed, :interesting, :failed, :error, :jobId)

/** selectResultsByTaskId */
select
      jr.passed as passedCount
    , jr.interesting as interestingCount
    , jr.failed as failedCount
    , jr.error as errorCount
from jcstress_results as jr
join jcstress_jobs as jj
    on jr.job_id = jj.id
where jj.task_id = :taskId
