import sys
import os

def runTest(i):
    rootFile = open("./test%s/root.om" % i, "r")
    rootLine = rootFile.readline()
    parts = rootLine.split("=")
    rootName = parts[1].strip()
    rootFile.close()

    print "TEST: Running test %i: %s." % (i, rootName)
    os.chdir("./test%i/" % i)
    if os.system("tjcc %s" % rootName) <> 0:
        print "Error: test case %i failed to compile." % i
        os.chdir("..")
        return
    if os.system("tjlink %s" % rootName) <> 0:
        print "Error: test case %i failed to link." % i
        os.chdir("..")
        return
    if os.system("tjdis %s.lp > actual" % rootName) <> 0:
        print "Error: test case %i failed to disassemble." % i
        os.chdir("..")
        return
    if os.system("diff expected actual") <> 0:
        print "Error: test case %i produced incorrect disassembly." % i
        os.chdir("..")
        return
    os.chdir("..")
    return

def main():
    if len(sys.argv) < 2:
        print "Usage: TestAll.py <number of tests>"
        return -1

    testMax = int(sys.argv[1])

    for i in range(1, testMax + 1):
        runTest(i)
    return

main()
