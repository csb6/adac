-- C97203C.ADA

-- CHECK THAT A CONDITIONAL ENTRY CALL CAN APPEAR IN PLACES WHERE A
-- SELECTIVE WAIT IS NOT ALLOWED.

-- PART 3: TASK BODY NESTED WITHIN A TASK.

-- WRG 7/15/86

WITH REPORT; USE REPORT;
PROCEDURE C97203C IS

BEGIN

     TEST ("C97203C", "CHECK THAT A CONDITIONAL ENTRY CALL CAN " &
                      "APPEAR IN PLACES WHERE A SELECTIVE WAIT " &
                      "IS NOT ALLOWED; CASE: TASK BODY NESTED " &
                      "WITHIN A TASK");

     DECLARE

          TASK T IS
               ENTRY E;
               ENTRY SYNCH;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT E;
          END T;

          TASK OUTER IS
               ENTRY E;
               ENTRY SYNCH;
          END OUTER;

          TASK BODY OUTER IS

               TASK TYPE INNER;

               INNER1 : INNER;

               TASK BODY INNER IS
               BEGIN
                    SELECT
                         T.E;
                         FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - " &
                                 "INNER (1)");
                    ELSE
                         T.SYNCH;
                    END SELECT;

                    SELECT
                         OUTER.E;
                         FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - " &
                                 "INNER (2)");
                    ELSE
                         OUTER.SYNCH;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED - INNER");
               END INNER;

               PACKAGE DUMMY IS
                    TYPE ACC_INNER IS ACCESS INNER;
                    INNER2 : ACC_INNER := NEW INNER;
               END DUMMY;

          BEGIN

               SELECT
                    T.E;
                    FAILED ("CONDITIONAL ENTRY CALL ACCEPTED - OUTER");
               ELSE
                    T.SYNCH;
               END SELECT;

               ACCEPT SYNCH;
               ACCEPT SYNCH;
               ACCEPT E;

          EXCEPTION

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - OUTER");

          END OUTER;

     BEGIN

          T.E;
          OUTER.E;

     END;

     RESULT;

END C97203C;
