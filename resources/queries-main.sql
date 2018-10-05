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

-- top-level tasks

/** updateTasksSeq */
update tasks_seq
set value = value + 1

/** selectNewTaskId */
select value as id
from tasks_seq

/** insertTask */
insert into tasks (id, start_date, state, comment)
values (:id, :startDate, :state, :comment)

/** updateTaskState */
update tasks
set   state = :state
where id = :id

/** updateTaskFinish */
update tasks
set   state = :state
    , finish_date = :finishDate
where id = :id
