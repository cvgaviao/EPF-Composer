run.bat			batch file to run headless build
build.properties	contains build options
allElements.xml		build script
customTargets.xml	build script
readme.txt		this file
build/			directory containing source pulled from CVS and build output - to be cleaned up after every build
templates/		directory containing template files from eclipse
workspace/		workspace created by Eclipse IDE as mentioned in 1.6


More Detailed Instructions

1. Set up Headless Build Environment

  1.1 Install JSDK is on the system and make sure java.exe is available on system PATH environment variable. [JAVAEXE]
  1.2 Install Apache ANT on the system and make sure the Ant binaries are accessible on the command line.
  1.3 Install IES or Open-Source Eclipse SDP is on the system and note the install location of Eclipse [ECLIPSEHOME]
  1.4 Obtain the plugins org.eclipse.pde.build and have this plugin in the Eclipse SDP [SCRIPTSHOME]
  1.5 Obtain the HEADLESS build scripts and put them it a directory by themselves [BUILDHOME]
  1.6 Start Eclipse SDP and define a workspace location [WORKSPACE].  Allow the IDE to start fully and then exit.  Only the workspace is needed.


2. Customize the Headless Build Scripts

  2.1 Modify run.bat file to suit your environment.
  2.2 Modify build.properties making sure that the following properties are addressed.
        product=
        buildDirectory=
        base=
        baseLocation=
        mapsRepo=
        mapsRoot=
        fetchTag=
  2.3 Modify allElements.xml
	Provide the correct top level feature being built.
	Provide the correct elementPath property in the allElementsDelegator target.

  2.4 Make sure the map files in org.eclipse.epf.releng/maps are accurate


