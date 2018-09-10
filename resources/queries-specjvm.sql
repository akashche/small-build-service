
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
