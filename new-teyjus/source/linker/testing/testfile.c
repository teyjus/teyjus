#include <string.h>
#include "file.h"
#include "datatypes.h"
#include "../tables/instructions.h"

void pname(char* name)
{
	Name tmp;
	tmp.size=strlen(name)+1;
	tmp.string=name;
	PutName(tmp);
}

void pgkind(int arity, char* name)
{
	PUT1(arity);
	pname(name);
}

void plkind(int arity){
	PUT1(arity);
}

void pgconst(int f, int p, int tes, char* name, int tsi)
{
	PUT1(f);
	PUT1(p);
	PUT1(tes);
	pname(name);
	PUT2(tsi);
}

void plconst(int f, int p, int tes, int tsi)
{
	PUT1(f);
	PUT1(p);
	PUT1(tes);
	PUT2(tsi);
}

void phconst(int tes, int tsi)
{
	PUT1(tes);
	PUT2(tsi);
}

void PutConstInd(ConstInd x)
{
	PUT1(x.gl_flag);
	PUT2(x.index);
}

int test()
{
	SetOutput("test");
	//Bytecode version number
	PUT4(3);
	//Module name
	pname("test");
	
	//Code size
	PUT4(200);
	
	//GKinds
	PUT2(3);
	pgkind(1,"kindt1");
	pgkind(0,"kindt2");
	pgkind(5,"kindt3");
	
	//LKinds
	PUT2(2);
	plkind(0);
	plkind(1);
	
	//TySkels
	PUT2(1);
	PUT1(4);
	PUT1(0);
	
	//GConsts
	PUT2(2);
	pgconst(50,50,1,"foo",0);
	pgconst(25,75,1,"bar",0);
	
	//LConsts
	PUT2(3);
	plconst(10,10,1,0);
	plconst(2,80,1,0);
	plconst(40,50,1,0);
	
	//HConsts
	PUT2(1);
	phconst(1,0);
	
	//Strings
	PUT2(5);
	pname("One");
	pname("Two");
	pname("Three");
	pname("Four");
	pname("Five");
	
	//ImplGoals
	PUT2(0);
	
	//HashTabs
	PUT2(0);
	
	//BvrTabs
	PUT2(0);
	
	//ImportTab
	//NextClause
	PUT2(1);
	ConstInd bar;
	bar.gl_flag=GLOBAL;
	bar.index=1;
	PutConstInd(bar);
	
	//ExportDef
	PUT2(1);
	ConstInd foo;
	foo.gl_flag=GLOBAL;
	foo.index=0;
	PutConstInd(foo);
	
	//LocalPred
	PUT2(1);
	ConstInd loc1;
	loc1.gl_flag=LOCAL;
	loc1.index=1;
	PutConstInd(loc1);
	
	PUT1(0);//Findcodefun
	
	//Search Tab
	PUT2(3);
	PutConstInd(bar);	PUTWORD(4);
	PutConstInd(foo);	PUTWORD(60);
	PutConstInd(loc1);	PUTWORD(100);
	
	//Accumulated Mods
	PUT1(1);
	pname("test2");
	PUT2(2);
	pname("kindt1");PUT1(GLOBAL);PUT2(0);
	pname("kindt4");PUT1(LOCAL);PUT2(1);
	PUT2(3);
	pname("bar");PUT1(GLOBAL);PUT2(1);
	pname("gold");PUT1(LOCAL);PUT2(0);
	pname("fish");PUT1(LOCAL);PUT2(1);
	
	//Imported Mods
	PUT1(0);
	
	//Bytecode
	int i;
	PUT1(fail);//00
	PUT1(switch_on_reg);PUT1(0);//04
	PUT4(16);//08
	PUT4(36);//12
	PUT1(try);PUT1(1);//16
	PUT4(36);//20
	PUT1(trust_ext);PUT1(1);PUT1(0);//24
	PUT1(fail);//28
	PUT1(fail);//32
	PUT1(unify_nil);//36
	
	for(i=40/4;i<60/4;i++)
	{
		PUT1(fail);
	}
	
	PUT1(unify_nil);//60
	
	for(i=64/4;i<=200/4;i++)
	{
		PUT1(fail);
	}
	
	PUT1(instr_end);
	
	return 0;
}

int test2()
{
	SetOutput("test2");
	//Bytecode version number
	PUT4(3);
	//Module name
	pname("test2");
	
	//Code size
	PUT4(200);
	
	//GKinds
	PUT2(2);
	pgkind(1,"kindt1");
	pgkind(1,"kindt4");
	
	//LKinds
	PUT2(2);
	plkind(0);
	plkind(1);
	
	//TySkels
	PUT2(1);
	PUT1(4);
	PUT1(0);
	
	//GConsts
	PUT2(3);
	pgconst(25,75,1,"bar",0);
	pgconst(10,10,1,"gold",0);
	pgconst(2,80,1,"fish",0);
	
	//LConsts
	PUT2(3);
	plconst(10,10,1,0);
	plconst(70,90,1,0);
	plconst(40,50,1,0);
	
	//HConsts
	PUT2(1);
	phconst(1,0);
	
	//Strings
	PUT2(5);
	pname("One");
	pname("Two");
	pname("Three");
	pname("Four");
	pname("Five");
	
	//ImplGoals
	PUT2(0);
	
	//HashTabs
	PUT2(0);
	
	//BvrTabs
	PUT2(0);
	
	//ImportTab
	//NextClause
	PUT2(1);
	ConstInd bar;
	bar.gl_flag=GLOBAL;
	bar.index=0;
	PutConstInd(bar);
	
	//ExportDef
	PUT2(2);
	ConstInd gold;
	gold.gl_flag=GLOBAL;
	gold.index=1;
	ConstInd fish;
	fish.gl_flag=GLOBAL;
	fish.index=2;
	PutConstInd(gold);
	PutConstInd(fish);
	
	//LocalPred
	PUT2(1);
	ConstInd loc1;
	loc1.gl_flag=LOCAL;
	loc1.index=1;
	PutConstInd(loc1);
	
	PUT1(0);//Findcodefun
	
	//Search Tab
	PUT2(4);
	PutConstInd(bar);	PUTWORD(4);
	PutConstInd(gold);	PUTWORD(20);
	PutConstInd(fish);	PUTWORD(80);
	PutConstInd(loc1);	PUTWORD(150);
	
	//Accumulated Mods
	PUT1(0);
	
	//Imported Mods
	PUT1(0);
	
	//Bytecode
	int i;
	PUT1(fail);//00
	PUT1(switch_on_reg);PUT1(0);//04
	PUT4(16);//08
	PUT4(36);//12
	PUT1(try);PUT1(1);//16
	PUT4(36);//20
	PUT1(trust_ext);PUT1(1);PUT1(0);//24
	PUT1(fail);//28
	PUT1(fail);//32
	PUT1(call);PUT1(1);//36
	PutConstInd(bar);//40
	
		
	for(i=44/4;i<80/4;i++)
	{
		PUT1(fail);
	}
	
	PUT1(switch_on_reg);PUT1(0);//80
	PUT4(92);//84
	PUT4(112);//88
	PUT1(try);PUT1(1);//92
	PUT4(112);//96
	PUT1(trust_ext);PUT1(1);PUT1(0);//100
	PUT1(fail);//104
	PUT1(fail);//108
	PUT1(unify_nil);//112
	
	for(i=116/4;i<=200/4;i++)
	{
		PUT1(fail);
	}
	
	PUT1(instr_end);
	
	return 0;
}

int main()
{
	test();
	test2();
}
