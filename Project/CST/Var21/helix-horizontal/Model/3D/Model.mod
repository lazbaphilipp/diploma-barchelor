'# MWS Version: Version 2020.1 - Oct 21 2019 - ACIS 29.0.1 -

'# length = mm
'# frequency = MHz
'# time = ns
'# frequency range: fmin = 860 fmax = 870
'# created = '[VERSION]2020.1|29.0.1|20191021[/VERSION]


'@ use template: Antenna - Wire.cfg

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
'set the units
With Units
    .Geometry "mm"
    .Frequency "MHz"
    .Voltage "V"
    .Resistance "Ohm"
    .Inductance "H"
    .TemperatureUnit  "Kelvin"
    .Time "ns"
    .Current "A"
    .Conductance "Siemens"
    .Capacitance "F"
End With

'----------------------------------------------------------------------------

'set the frequency range
Solver.FrequencyRange "860", "870"

'----------------------------------------------------------------------------

Plot.DrawBox True

With Background
     .Type "Normal"
     .Epsilon "1.0"
     .Mu "1.0"
     .XminSpace "0.0"
     .XmaxSpace "0.0"
     .YminSpace "0.0"
     .YmaxSpace "0.0"
     .ZminSpace "0.0"
     .ZmaxSpace "0.0"
End With

With Boundary
     .Xmin "expanded open"
     .Xmax "expanded open"
     .Ymin "expanded open"
     .Ymax "expanded open"
     .Zmin "expanded open"
     .Zmax "expanded open"
     .Xsymmetry "none"
     .Ysymmetry "none"
     .Zsymmetry "none"
End With

' switch on FD-TET setting for accurate farfields

FDSolver.ExtrudeOpenBC "True"

Mesh.FPBAAvoidNonRegUnite "True"
Mesh.ConsiderSpaceForLowerMeshLimit "False"
Mesh.MinimumStepNumber "5"
Mesh.RatioLimit "20"
Mesh.AutomeshRefineAtPecLines "True", "10"

With MeshSettings
     .SetMeshType "Hex"
     .Set "RatioLimitGeometry", "20"
     .Set "EdgeRefinementOn", "1"
     .Set "EdgeRefinementRatio", "10"
End With

With MeshSettings
     .SetMeshType "Tet"
     .Set "VolMeshGradation", "1.5"
     .Set "SrfMeshGradation", "1.5"
End With

With MeshSettings
     .SetMeshType "HexTLM"
     .Set "RatioLimitGeometry", "20"
End With

PostProcess1D.ActivateOperation "vswr", "true"
PostProcess1D.ActivateOperation "yz-matrices", "true"

With MeshSettings
     .SetMeshType "Srf"
     .Set "Version", 1
End With
IESolver.SetCFIEAlpha "1.000000"

With FarfieldPlot
	.ClearCuts ' lateral=phi, polar=theta
	.AddCut "lateral", "0", "1"
	.AddCut "lateral", "90", "1"
	.AddCut "polar", "90", "1"
End With

'----------------------------------------------------------------------------

Dim sDefineAt As String
sDefineAt = "868"
Dim sDefineAtName As String
sDefineAtName = "868"
Dim sDefineAtToken As String
sDefineAtToken = "f="
Dim aFreq() As String
aFreq = Split(sDefineAt, ";")
Dim aNames() As String
aNames = Split(sDefineAtName, ";")

Dim nIndex As Integer
For nIndex = LBound(aFreq) To UBound(aFreq)

Dim zz_val As String
zz_val = aFreq (nIndex)
Dim zz_name As String
zz_name = sDefineAtToken & aNames (nIndex)

' Define E-Field Monitors
With Monitor
    .Reset
    .Name "e-field ("& zz_name &")"
    .Dimension "Volume"
    .Domain "Frequency"
    .FieldType "Efield"
    .MonitorValue  zz_val
    .Create
End With

' Define H-Field Monitors
With Monitor
    .Reset
    .Name "h-field ("& zz_name &")"
    .Dimension "Volume"
    .Domain "Frequency"
    .FieldType "Hfield"
    .MonitorValue  zz_val
    .Create
End With

' Define Farfield Monitors
With Monitor
    .Reset
    .Name "farfield ("& zz_name &")"
    .Domain "Frequency"
    .FieldType "Farfield"
    .MonitorValue  zz_val
    .ExportFarfieldSource "False"
    .Create
End With

Next

'----------------------------------------------------------------------------

With MeshSettings
     .SetMeshType "Hex"
     .Set "Version", 1%
End With

With Mesh
     .MeshType "PBA"
End With

'set the solver type
ChangeSolverType("HF Time Domain")

'----------------------------------------------------------------------------




'@ execute macro: Construct\Coils\3D Linear Helical Spiral

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Dim scst_torrus_ri As Double, scst_torrus_ra As Double,cst_xxx As Double, scst_torrus_phi As Double, scst_torrus_h As Double
 Dim scst_torrus_N As Integer, cst_result As Integer,  sCurveName As String, scst_clock As Integer, cst_clock As Integer
 Dim cst_torrus_N As Double, cst_torrus_phi As Double, cst_torrus_h As Double, cst_torrus_ra As Double, cst_torrus_ri As Double
 Dim first As String,  seconditem As String,cName As String, clock_yes_no As Integer, cClock As Integer

cst_result = -1%
cName = "Linear_Spiral"

If (cst_result =0) Then Exit All   ' if cancel/help is clicked, exit all

MakeSureParameterExists(cName+"_cst_torrus_h",10)
MakeSureParameterExists(cName+"_cst_torrus_ra",5)
MakeSureParameterExists(cName+"_cst_torrus_ri",5)
MakeSureParameterExists(cName+"_cst_torrus_phi",30)
MakeSureParameterExists(cName+"_cst_torrus_N",10)
MakeSureParameterExists(cName+"_cst_torrus_orientation",0)

If (RestoreDoubleParameter(cName+"_cst_torrus_phi") <= 0) Then
	ReportWarningToWindow "Angle phi is not positive. Coil not constructed."
	Exit All
End If

 With Brick
		.Reset
		.Name "dummy_solid_trapez"
		.Component "Dummy_spiral_coil"
		.Material "PEC"
		.Xrange "0", cName+"_cst_torrus_h"
		.Yrange "0", cName+"_cst_torrus_ra"
		.Zrange "0", cName+"_cst_torrus_phi"
		.Create
	End With
	Component.Delete "Dummy_spiral_coil"



	cst_torrus_N= restoredoubleparameter (cName+"_cst_torrus_N")
	cst_torrus_h= restoredoubleparameter (cName+"_cst_torrus_h")
	cst_torrus_ra= restoredoubleparameter (cName+"_cst_torrus_ra")
	cst_torrus_ri= restoredoubleparameter (cName+"_cst_torrus_ri")
	cst_torrus_phi= restoredoubleparameter (cName+"_cst_torrus_phi")
	cst_clock = restoredoubleparameter (cName+"_cst_torrus_orientation")


 ' Begin construction

 clock_yes_no = CInt(cst_clock)

	On Error GoTo Curve_Exists
 	Curve.NewCurve "3D-Linear-Spiral"
 	Curve_Exists:
	On Error GoTo 0
 	sCurveName = cName '"3dpolygon_1"
 	With    Polygon3D
  		.Reset
  		.Name sCurveName
  		.Curve "3D-Linear-Spiral"
        ' the upper limit takes the numerical inaccuracies into account. The logic of the following loop is as follows:
        ' We go complete the number of turns the user has specified. If 360 modulo cst_torrus_phi is not equal to zero
        ' (which shouldn't be the case usually) we insert the last segment only if the overlap is less than half the length
        ' of the segment.
        	.setinterpolation "Spline"
  		For cst_xxx = 0  To    cst_torrus_N *2*Pi+(cst_torrus_phi*pi/180)/2 STEP  cst_torrus_phi*pi/180
  			If clock_yes_no = 1 Then
	 			.Point cst_torrus_ra*Sin(cst_xxx), cst_torrus_ri*Cos(cst_xxx) , cst_torrus_h*cst_xxx/(2*pi* cst_torrus_N)
	 		Else
	 			.Point -cst_torrus_ra*Sin(cst_xxx), cst_torrus_ri*Cos(cst_xxx) , cst_torrus_h*cst_xxx/(2*pi* cst_torrus_N)
	 		End If
  		Next cst_xxx
  		.Create
 	End With


'@ pick end point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickCurveEndpointFromId "3D-Linear-Spiral:Linear_Spiral", "1" 


'@ align wcs with point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
WCS.AlignWCSWithSelected "Point" 


'@ define curve circle: 3D-Linear-Spiral:circle1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Circle
     .Reset 
     .Name "circle1" 
     .Curve "3D-Linear-Spiral" 
     .Radius "0.5" 
     .Xcenter "0.0" 
     .Ycenter "0.0" 
     .Segments "0" 
     .Create
End With


'@ new component: component1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Component.New "component1" 


'@ define sweepprofile: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With SweepCurve
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .Twistangle "0.0" 
     .Taperangle "0.0" 
     .ProjectProfileToPathAdvanced "True" 
     .CutEndOff "True" 
     .DeleteProfile "True" 
     .DeletePath "True" 
     .Path "3D-Linear-Spiral:Linear_Spiral" 
     .Curve "3D-Linear-Spiral:circle1" 
     .Create
End With


'@ define cylinder: component1:solid2

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Cylinder 
     .Reset 
     .Name "solid2" 
     .Component "component1" 
     .Material "PEC" 
     .OuterRadius "0.5" 
     .InnerRadius "0.0" 
     .Axis "z" 
     .Zrange "-2.5", "-0.51" 
     .Xcenter "0.5001" 
     .Ycenter "0" 
     .Segments "0" 
     .Create 
End With 


'@ pick face

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickFaceFromId "component1:solid2", "3" 


'@ pick face

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickFaceFromId "component1:solid1", "2" 


'@ define loft: component1:solid3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Loft 
     .Reset 
     .Name "solid3" 
     .Component "component1" 
     .Material "PEC" 
     .Tangency "0.100000" 
     .Minimizetwist "true" 
     .CreateNew 
End With 


'@ boolean add shapes: component1:solid1, component1:solid2

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid1", "component1:solid2" 


'@ boolean add shapes: component1:solid1, component1:solid3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid1", "component1:solid3" 


'@ define brick: component1:solid2

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid2" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "0", "-2" 
     .Yrange "0.65", "34.2" 
     .Zrange "-24", "-1" 
     .Create
End With


'@ define brick: component1:solid3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid3" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "0", "-2" 
     .Yrange "-5.775", "-0.65" 
     .Zrange "-24", "-1" 
     .Create
End With


'@ define brick: component1:solid4

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid4" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "0", "-2" 
     .Yrange "3.55", "34.2" 
     .Zrange "-1", "17" 
     .Create
End With


'@ define brick: component1:solid5

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid5" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "0", "-2" 
     .Yrange "-0.65", "0.65" 
     .Zrange "-5", "-24" 
     .Create
End With


'@ define brick: component1:solid6

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid6" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "-2", "-1.5" 
     .Yrange "-0.65", "0.65" 
     .Zrange "-5", "-1" 
     .Create
End With


'@ boolean add shapes: component1:solid2, component1:solid3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid2", "component1:solid3" 


'@ boolean add shapes: component1:solid4, component1:solid5

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid4", "component1:solid5" 


'@ boolean add shapes: component1:solid4, component1:solid6

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid4", "component1:solid6" 


'@ boolean add shapes: component1:solid2, component1:solid4

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:solid2", "component1:solid4" 


'@ pick circle center point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickCirclecenterFromId "component1:solid1", "4" 


'@ pick mid point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickMidpointFromId "component1:solid2", "13" 


'@ define discrete port: 1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With DiscretePort 
     .Reset 
     .PortNumber "1" 
     .Type "SParameter" 
     .Label "" 
     .Folder "" 
     .Impedance "50.0" 
     .VoltagePortImpedance "0.0" 
     .Voltage "1.0" 
     .Current "1.0" 
     .SetP1 "True", "0.5001", "0", "-2.5" 
     .SetP2 "True", "0", "0", "-5" 
     .InvertDirection "False" 
     .LocalCoordinates "True" 
     .Monitor "True" 
     .Radius "0.0" 
     .Wire "" 
     .Position "end1" 
     .Create 
End With


'@ define time domain solver parameters

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Mesh.SetCreator "High Frequency" 

With Solver 
     .Method "Hexahedral"
     .CalculationType "TD-S"
     .StimulationPort "All"
     .StimulationMode "All"
     .SteadyStateLimit "-40"
     .MeshAdaption "False"
     .AutoNormImpedance "False"
     .NormingImpedance "50"
     .CalculateModesOnly "False"
     .SParaSymmetry "False"
     .StoreTDResultsInCache  "False"
     .FullDeembedding "False"
     .SuperimposePLWExcitation "False"
     .UseSensitivityAnalysis "False"
End With


'@ define time domain solver acceleration

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Solver 
     .UseParallelization "True"
     .MaximumNumberOfThreads "128"
     .MaximumNumberOfCPUDevices "2"
     .RemoteCalculation "False"
     .UseDistributedComputing "False"
     .MaxNumberOfDistributedComputingPorts "64"
     .DistributeMatrixCalculation "True"
     .MPIParallelization "False"
     .HardwareAcceleration "True"
     .MaximumNumberOfGPUs "2"
End With
UseDistributedComputingForParameters "False"
MaxNumberOfDistributedComputingParameters "2"
UseDistributedComputingMemorySetting "False"
MinDistributedComputingMemoryLimit "0"
UseDistributedComputingSharedDirectory "False"


'@ set PBA version

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Discretizer.PBAVersion "2019102120"

'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "3D" 
     .Vary "angle1" 
     .Theta "90" 
     .Phi "90" 
     .Step "5" 
     .Step2 "5" 
     .SetLockSteps "True" 
     .SetPlotRangeOnly "False" 
     .SetThetaStart "0" 
     .SetThetaEnd "180" 
     .SetPhiStart "0" 
     .SetPhiEnd "360" 
     .SetTheta360 "False" 
     .SymmetricRange "False" 
     .SetTimeDomainFF "False" 
     .SetFrequency "-1" 
     .SetTime "0" 
     .SetColorByValue "True" 
     .DrawStepLines "False" 
     .DrawIsoLongitudeLatitudeLines "False" 
     .ShowStructure "True" 
     .ShowStructureProfile "True" 
     .SetStructureTransparent "False" 
     .SetFarfieldTransparent "False" 
     .AspectRatio "Free" 
     .ShowGridlines "True" 
     .SetSpecials "enablepolarextralines" 
     .SetPlotMode "Directivity" 
     .Distance "1" 
     .UseFarfieldApproximation "True" 
     .SetScaleLinear "False" 
     .SetLogRange "40" 
     .SetLogNorm "0" 
     .DBUnit "0" 
     .SetMaxReferenceMode "abs" 
     .EnableFixPlotMaximum "False" 
     .SetFixPlotMaximumValue "1" 
     .SetInverseAxialRatio "False" 
     .SetAxesType "user" 
     .SetAntennaType "unknown" 
     .Phistart "1.000000e+00", "0.000000e+00", "0.000000e+00" 
     .Thetastart "0.000000e+00", "0.000000e+00", "1.000000e+00" 
     .PolarizationVector "0.000000e+00", "1.000000e+00", "0.000000e+00" 
     .SetCoordinateSystemType "spherical" 
     .SetAutomaticCoordinateSystem "True" 
     .SetPolarizationType "Linear" 
     .SlantAngle 0.000000e+00 
     .Origin "bbox" 
     .Userorigin "0.000000e+00", "0.000000e+00", "0.000000e+00" 
     .SetUserDecouplingPlane "False" 
     .UseDecouplingPlane "False" 
     .DecouplingPlaneAxis "X" 
     .DecouplingPlanePosition "0.000000e+00" 
     .LossyGround "False" 
     .GroundEpsilon "1" 
     .GroundKappa "0" 
     .EnablePhaseCenterCalculation "False" 
     .SetPhaseCenterAngularLimit "3.000000e+01" 
     .SetPhaseCenterComponent "boresight" 
     .SetPhaseCenterPlane "both" 
     .ShowPhaseCenter "True" 
     .ClearCuts 
     .AddCut "lateral", "0", "1"  
     .AddCut "lateral", "90", "1"  
     .AddCut "polar", "90", "1"  

     .StoreSettings
End With 


'@ switch bounding box

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawBox "False" 


'@ switch working plane

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawWorkplane "false" 


'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "Polar" 
     .Vary "angle1" 
     .Theta "90" 
     .Phi "90" 
     .Step "1" 
     .Step2 "1" 
     .SetLockSteps "True" 
     .SetPlotRangeOnly "False" 
     .SetThetaStart "0" 
     .SetThetaEnd "180" 
     .SetPhiStart "0" 
     .SetPhiEnd "360" 
     .SetTheta360 "False" 
     .SymmetricRange "False" 
     .SetTimeDomainFF "False" 
     .SetFrequency "-1" 
     .SetTime "0" 
     .SetColorByValue "True" 
     .DrawStepLines "False" 
     .DrawIsoLongitudeLatitudeLines "False" 
     .ShowStructure "True" 
     .ShowStructureProfile "True" 
     .SetStructureTransparent "False" 
     .SetFarfieldTransparent "True" 
     .AspectRatio "Free" 
     .ShowGridlines "True" 
     .SetSpecials "enablepolarextralines" 
     .SetPlotMode "Gain" 
     .Distance "1" 
     .UseFarfieldApproximation "True" 
     .SetScaleLinear "False" 
     .SetLogRange "40" 
     .SetLogNorm "0" 
     .DBUnit "0" 
     .SetMaxReferenceMode "abs" 
     .EnableFixPlotMaximum "False" 
     .SetFixPlotMaximumValue "1" 
     .SetInverseAxialRatio "False" 
     .SetAxesType "user" 
     .SetAntennaType "unknown" 
     .Phistart "1.000000e+00", "0.000000e+00", "0.000000e+00" 
     .Thetastart "0.000000e+00", "0.000000e+00", "1.000000e+00" 
     .PolarizationVector "0.000000e+00", "1.000000e+00", "0.000000e+00" 
     .SetCoordinateSystemType "spherical" 
     .SetAutomaticCoordinateSystem "True" 
     .SetPolarizationType "Linear" 
     .SlantAngle 0.000000e+00 
     .Origin "bbox" 
     .Userorigin "0.000000e+00", "0.000000e+00", "0.000000e+00" 
     .SetUserDecouplingPlane "False" 
     .UseDecouplingPlane "False" 
     .DecouplingPlaneAxis "X" 
     .DecouplingPlanePosition "0.000000e+00" 
     .LossyGround "False" 
     .GroundEpsilon "1" 
     .GroundKappa "0" 
     .EnablePhaseCenterCalculation "False" 
     .SetPhaseCenterAngularLimit "3.000000e+01" 
     .SetPhaseCenterComponent "boresight" 
     .SetPhaseCenterPlane "both" 
     .ShowPhaseCenter "True" 
     .ClearCuts 
     .AddCut "lateral", "0", "1"  
     .AddCut "lateral", "90", "1"  
     .AddCut "polar", "90", "1"  

     .StoreSettings
End With 


