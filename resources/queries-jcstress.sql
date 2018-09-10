
-- jcstress_runs

/** updateRunsId */
update jcstress_runs_seq
set value = value + 1

/** selectRunsId */
select value as id
from jcstress_runs_seq

/** insertRun */
insert into jcstress_runs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateRunFinish */
update jcstress_runs
set   state = :state
    , finish_date = :finishDate
    , passed = :passed
    , passedDiff = :passedDiff
    , interesting = :interesting
    , interestingDiff = :interestingDiff
    , failed = :failed
    , failedDiff = :failedDiff
    , error = :error
    , errorDiff = :errorDiff
where id = :id
