

int main(int argc, char* argv[])
{
	//TODO Check more arguments and validate them.
	if(argc<2)
	{
		printf("Missing argument module name");
		exit(0);
	}
	
	InitAll();
	LoadTopModule(argv[1]);
	WriteAll();
}