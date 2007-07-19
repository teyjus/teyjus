#include "../simulator/mctypes.h"

/***************************************************************************/
/*                        solving queries                                  */
/***************************************************************************/
/* register the current heap top and the next cell as the positions of     */
/* the type of query and the query; increase the heap top correspondingly  */
void QUERY_setTypeAndTermLocation();

/* solve query */
int QUERY_solveQuery();

/* display answers */
int QUERY_showAnswers();
void QUERY_setQueryFreeVariables();
Boolean QUERY_queryHasVars();

