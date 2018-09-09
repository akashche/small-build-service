
-- top-level tasks

/** tasksUpdateId */
update tasks_seq
set value = value + 1

/** tasksSelectId */
select value as id
from tasks_seq

/** tasksInsert */
insert into tasks (id, start_date, state, repository, revision, comment)
values (:id, :startDate, :state, :repository, :revision, :comment)

/** tasksUpdateState */
update tasks
set state = :state
where id = :id

/** tasksUpdateFinish */
update tasks
set   state = :state
    , finish_date = :finishDate
where id = :id
