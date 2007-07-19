#include "../simulator/mctypes.h"

/***************************************************************************/
/*                       system initialization                             */
/***************************************************************************/
int FRONT_systemInit(int memSize);

/***************************************************************************/
/*                       link and load                                     */
/***************************************************************************/
int FRONT_link(char* modName);
int FRONT_load(char* modName, int index);

/***************************************************************************/
/*                simulator memory partition                               */
/***************************************************************************/
int FRONT_simulatorInit();
int FRONT_simulatorReInit(Boolean inDoInitializeImports);

/***************************************************************************/
/*                   install and open module                               */
/***************************************************************************/
int FRONT_topModuleInstall();
int FRONT_moduleInstall(int ind);
int FRONT_initModuleContext();

