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

/** updateRunsId */
update specjvm_runs_seq
set value = value + 1

/** selectRunsId */
select value as id
from specjvm_runs_seq

/** insertRun */
insert into specjvm_runs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateRunFinish */
update specjvm_runs
set   state = :state
    , finish_date = :finishDate
    , total_time_seconds = :totalTimeSeconds
    , relative_total_time = :relativeTotalTime
where id = :id


-- specjvm_results

/** updateResultsId */
update specjvm_runs_seq
set value = value + 1

/** selectResultsId */
select value as id
from specjvm_runs_seq

/** insertResult */
insert into specjvm_results (id, name, mode, counts, score, error, units, run_id)
values (:id, :name, :mode, :counts, :score, :error, :units, :runId)

-- specjvm_diffs

/** updateDiffsId */
update specjvm_diffs_seq
set value = value + 1

/** selectDiffsId */
select value as id
from specjvm_diffs_seq

/** insertDiff */
insert into specjvm_diffs (id, name, relative_score, run_id)
values (:id, :name, :relativeScore, :runId)
