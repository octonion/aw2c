#include "alw.h"

int nesting = 0;

void alw_trace_procedure_called (alw_loc call_loc, const char *procedure_name)
{
    alw_warning(call_loc, "%*s--> %s", nesting, "", procedure_name);
    nesting += 4;
}

void alw_trace_procedure_entered (alw_loc procedure_loc, const char *procedure_name)
{
    alw_warning(procedure_loc, "%*s%s", nesting, "", procedure_name);
}

void alw_trace_procedure_exited (alw_loc call_loc, const char *procedure_name)
{
    nesting -= 4;
    alw_warning(call_loc, "%*s<-- %s", nesting, "", procedure_name);
}
