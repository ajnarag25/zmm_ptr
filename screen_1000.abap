PROCESS BEFORE OUTPUT.

MODULE %_INIT_PBO.

MODULE %_PBO_REPORT.

MODULE %_PF_STATUS.

MODULE %_A_PERIOD.

MODULE %_END_OF_PBO.

PROCESS AFTER INPUT.

  MODULE %_BACK AT EXIT-COMMAND.

  MODULE %_INIT_PAI.

CHAIN.
  FIELD AGG     .
  FIELD CEM     .
  FIELD RSB     .
    MODULE %_RADIOBUTTON_GROUP_RAD                           .
ENDCHAIN.

CHAIN.
  FIELD  A_PERIOD-LOW.
  FIELD  A_PERIOD-HIGH.
  MODULE %_A_PERIOD.
ENDCHAIN.


CHAIN.
  FIELD AGG     .
  FIELD CEM     .
  FIELD RSB     .
  FIELD  A_PERIOD-LOW.
  FIELD  A_PERIOD-HIGH.
    MODULE %_BLOCK_1000000.
ENDCHAIN.

CHAIN.
  FIELD AGG     .
  FIELD CEM     .
  FIELD RSB     .
  FIELD  A_PERIOD-LOW.
  FIELD  A_PERIOD-HIGH.
  MODULE %_END_OF_SCREEN.
  MODULE %_OK_CODE_1000.
ENDCHAIN.
