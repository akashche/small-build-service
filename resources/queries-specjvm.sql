
/** updateId */
update specjvm_runs_seq
set value = value + 1

/** selectId */
select value as id
from specjvm_runs_seq

/** insert */
insert into specjvm_runs (id, start_date, state, task_id)
values (:id, :startDate, :state, :taskId)

/** updateState */
update specjvm_runs
set state = :state
where id = :id

/** updateFinish */
update specjvm_runs
set   state = :state
    , finish_date = :finishDate
    , total_time_seconds = :totalTimeSeconds
where id = :id
