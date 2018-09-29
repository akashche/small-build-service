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

-- jdkbuild_runs

/** updateJobsSeq */
update jdkbuild_jobs_seq
set value = value + 1

/** selectNewJobId */
select value as id
from jdkbuild_jobs_seq

/** insertJob */
insert into jdkbuild_jobs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateJobRepo */
update jdkbuild_jobs
set   repository = :repository
    , revision = :revision
where id = :id

/** updateJobState */
update jdkbuild_jobs
set   state = :state
where id = :id

/** updateJobFinish */
update jdkbuild_jobs
set   state = :state
    , finish_date = :finishDate
where id = :id
