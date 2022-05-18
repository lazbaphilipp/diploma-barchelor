'# MWS Version: Version 2020.1 - Oct 21 2019 - ACIS 29.0.1 -

'# length = mm
'# frequency = MHz
'# time = ns
'# frequency range: fmin = 800 fmax = 900
'# created = '[VERSION]2020.1|29.0.1|20191021[/VERSION]


'@ use template: Antenna - helix normal mode.cfg

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
Solver.FrequencyRange "800", "900"
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
sDefineAt = "800;850;900"
Dim sDefineAtName As String
sDefineAtName = "800;850;900"
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

'@ switch bounding box

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawBox "False"

'@ execute macro: Construct\Coils\3D Toroidal Coil - circular core

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Dim cst_torrus_r As Double
 Dim cst_torrus_phi As Double
 Dim cst_torrus_alpha As Double
 Dim cst_torrus_i As Integer
 Dim cst_torrus_psi As Double, cst_torrus_g As Double
 Dim cst_torrus_x As Double, cst_torrus_y As Double, cst_torrus_h As Double
 Dim cst_torrus_slope As Double
 Dim cst_torrus_N As Integer
 Dim cst_torrus_wire_d As Double
 Dim cst_torrus_r_torrus As Double
 Dim cst_torrus_slope_option As Integer
Dim scst_torrus_r_torrus As String
Dim scst_torrus_r As String
Dim scst_torrus_phi As String
Dim scst_torrus_g As String
Dim scst_torrus_slope As String
Dim scst_torrus_N As String
Dim scst_torrus_wire_d As String
Dim cst_result As Integer
cst_result = 0%
scst_torrus_r_torrus = "8."
scst_torrus_r = "10."
scst_torrus_phi = "30."
scst_torrus_g = "60."
scst_torrus_slope = "5."
scst_torrus_N = "7"
scst_torrus_wire_d = "1.5"
cst_torrus_slope_option = 0%
If (cst_result =0) Then Exit All   ' if cancel/help is clicked, exit all
On Error Resume Next
 'WCS.ActivateWCS "global"
 Solid.Delete "Ferrite:torrus" 
 Solid.Delete "Ferrite:dummy_zyl" 
 Solid.delete "Wire_material:windings"
 Curve.DeleteCurve "torrus_curve" 
 Curve.DeleteCurve "torrus_wire" 
 On Error GoTo 0
'EndHide
If WCS.DoesExist ("WCS_torrus") Then
	WCS.reStore "WCS_torrus"
Else
	WCS.Store "WCS_torrus"
End If
WCS.restore "WCS_torrus"
 cst_torrus_r_torrus = Evaluate(scst_torrus_r_torrus)
 cst_torrus_r =  Evaluate(scst_torrus_r)
 cst_torrus_phi =  Evaluate(scst_torrus_phi)
 cst_torrus_g =  Evaluate(scst_torrus_g)
 cst_torrus_slope =  Evaluate(scst_torrus_slope)
 cst_torrus_N =  Evaluate(scst_torrus_N)
 cst_torrus_wire_d =  Evaluate(scst_torrus_wire_d)
 ' Begin construction
 'slope 
 If cst_torrus_slope_option = 1 Then 'height option
  cst_torrus_alpha = Atn(cst_torrus_slope/(2*pi*cst_torrus_r))*180/pi
 Else 'degree option
  cst_torrus_alpha = cst_torrus_slope 
 End If
 
 'check for penetration
 If cst_torrus_r_torrus + cst_torrus_wire_d > cst_torrus_r Then
  MsgBox "Wire radius +  Torrus Core-radius > Winding Core-Radius!"
  Exit All
 End If 
 'existing items?
 
 Curve.NewCurve "torrus_curve" 
 Curve.NewCurve "torrus_wire" 
 With Material
     .Reset 
     .Name "Ferrite"
     .FrqType "hf" 
     .Type "Normal" 
     .Epsilon "1.0" 
     .Mu "1.0" 
     .Kappa "0.0" 
     .TanD "0.0" 
     .TanDFreq "0.0" 
     .TanDGiven "False" 
     .TanDModel "ConstTanD" 
     .KappaM "0.0" 
     .TanDM "0.0" 
     .TanDMFreq "0.0" 
     .TanDMGiven "False" 
     .DispModelEps "None" 
     .DispModelMu "None" 
     .Rho "0.0" 
     .Colour "0.501961", "0.501961", "0.501961" 
     .Wireframe "False" 
     .Transparency "0" 
     .Create
 End With 
 With Material
     .Reset 
     .Name "Wire_material"
     .FrqType "hf" 
     .Type "Pec" 
     .Rho "0.0" 
     .Colour "1", "0.501961", "0.752941" 
     .Wireframe "False" 
     .Transparency "0" 
     .Create
 End With 
 With Polygon3D 
     .Reset 
     .Name "torrus_3dpolygon" 
     .Curve "torrus_curve" 
     For cst_torrus_i = 0 To cst_torrus_N*(360/cst_torrus_phi)
      'x = r*Cos(phi*i*pi/180)
      cst_torrus_y = cst_torrus_r*Sin(cst_torrus_phi*cst_torrus_i*pi/180)
      cst_torrus_h = cst_torrus_r*cst_torrus_phi*cst_torrus_i*pi/180*Tan(cst_torrus_alpha*pi/180) 
      cst_torrus_psi = cst_torrus_h/(cst_torrus_g-cst_torrus_r*Sin(cst_torrus_phi*pi/180))
     
      cst_torrus_x = cst_torrus_g-  _
         (cst_torrus_g-cst_torrus_r*Cos(cst_torrus_phi*cst_torrus_i*pi/180))*Cos(cst_torrus_psi)	'x-correction
      cst_torrus_h = (cst_torrus_g-cst_torrus_r*  _
          Cos(cst_torrus_phi*cst_torrus_i*pi/180))*Sin(cst_torrus_psi)     	'height- correction
      .Point cst_torrus_x, cst_torrus_y,cst_torrus_h 
     Next cst_torrus_i
     .Create 
 End With 
 With Cylinder 
     .Reset 
     .Name "dummy_zyl" 
     .Component "Ferrite" 
     .Material "Ferrite" 
     .OuterRadius cst_torrus_r_torrus
     .InnerRadius "0" 
     .Axis "z"
     .Zrange "0", "12" 
     .Xcenter "0" 
     .Ycenter "0" 
     .Create 
 End With 
 Pick.PickFaceFromId "Ferrite:dummy_zyl", "1" 
 Pick.AddEdge cst_torrus_g, "-1", "0", cst_torrus_g, "1", "0" 
 With Rotate 
     .Reset 
     .Name Solid.GetNextFreeName
     .Component "Ferrite" 
     .Material "Ferrite" 
     .Mode "Picks" 
     .Angle "360" 
     .Height "0.0" 
     .RadiusRatio "1.0" 
     .NSteps "0" 
     .Create 
 End With 
 Solid.Delete "Ferrite:dummy_zyl"
 'WCS.ActivateWCS "local"
 WCS.reStore "WCS_torrus"
 Pick.PickCurveEndpointFromId "torrus_curve:torrus_3dpolygon", "1"
 WCS.AlignWCSWithSelectedPoint
 'With WCS
     '.SetNormal "0.0", "0.0", "1.0"
     '.SetOrigin cst_torrus_r, "0.0", "0.0"
     '.SetUVector "1.0", "0.0", "0.0"
 'End With
 WCS.RotateWCS "u", 90+( cst_torrus_alpha) '*180/pi)	'rotate Coordinatesystem
 With Circle
     .Reset 
     .Name "wire" 
     .Curve "torrus_wire" 
     .Radius cst_torrus_wire_d 
     .Xcenter "0." 
     .Ycenter "0." 
     .Create
 End With
 With SweepCurve
     .Reset
     .Name Solid.GetNextFreeName
     .Component "Wire_material"
     .Material "Wire_material"
     .Twistangle "0.0"
     .Taperangle "0.0"
     .ProjectProfileToPathAdvanced "False"
     .Path "torrus_curve:torrus_3dpolygon"
     .Curve "torrus_wire:wire"
     .Create
End With
 Curve.DeleteCurve "torrus_curve"
 Curve.DeleteCurve "torrus_wire"
 
 WCS.ActivateWCS "global"
 Pick.ClearAllPicks

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

'@ define curve circle: 3D-Linear-Spiral:Helix Base

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Circle
     .Reset 
     .Name "Helix Base" 
     .Curve "3D-Linear-Spiral" 
     .Radius "0.5" 
     .Xcenter "0" 
     .Ycenter "0" 
     .Segments "0" 
     .Create
End With

'@ new component: component1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Component.New "component1"

'@ define sweepprofile: component1:Helix

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With SweepCurve
     .Reset 
     .Name "Helix" 
     .Component "component1" 
     .Material "PEC" 
     .Twistangle "0.0" 
     .Taperangle "0.0" 
     .ProjectProfileToPathAdvanced "True" 
     .CutEndOff "True" 
     .DeleteProfile "True" 
     .DeletePath "True" 
     .Path "3D-Linear-Spiral:Linear_Spiral" 
     .Curve "3D-Linear-Spiral:Helix Base" 
     .Create
End With

'@ define cylinder: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Cylinder 
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .OuterRadius "0,5" 
     .InnerRadius "0" 
     .Axis "z" 
     .Zrange "-3", "0" 
     .Xcenter "-0,5" 
     .Ycenter "-0.1" 
     .Segments "0" 
     .Create 
End With

'@ rename block: component1:solid1 to: component1:Feed

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:solid1", "Feed"

'@ clear picks

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.ClearAllPicks

'@ pick edge

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickEdgeFromId "component1:Feed", "2", "2"

'@ execute macro: Construct\Coils\3D Toroidal Coil - circular core

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Dim cst_torrus_r As Double
 Dim cst_torrus_phi As Double
 Dim cst_torrus_alpha As Double
 Dim cst_torrus_i As Integer
 Dim cst_torrus_psi As Double, cst_torrus_g As Double
 Dim cst_torrus_x As Double, cst_torrus_y As Double, cst_torrus_h As Double
 Dim cst_torrus_slope As Double
 Dim cst_torrus_N As Integer
 Dim cst_torrus_wire_d As Double
 Dim cst_torrus_r_torrus As Double
 Dim cst_torrus_slope_option As Integer
Dim scst_torrus_r_torrus As String
Dim scst_torrus_r As String
Dim scst_torrus_phi As String
Dim scst_torrus_g As String
Dim scst_torrus_slope As String
Dim scst_torrus_N As String
Dim scst_torrus_wire_d As String
Dim cst_result As Integer
cst_result = 0%
scst_torrus_r_torrus = "8."
scst_torrus_r = "10."
scst_torrus_phi = "30."
scst_torrus_g = "60."
scst_torrus_slope = "5."
scst_torrus_N = "7"
scst_torrus_wire_d = "1.5"
cst_torrus_slope_option = 0%
If (cst_result =0) Then Exit All   ' if cancel/help is clicked, exit all
On Error Resume Next
 'WCS.ActivateWCS "global"
 Solid.Delete "Ferrite:torrus" 
 Solid.Delete "Ferrite:dummy_zyl" 
 Solid.delete "Wire_material:windings"
 Curve.DeleteCurve "torrus_curve" 
 Curve.DeleteCurve "torrus_wire" 
 On Error GoTo 0
'EndHide
If WCS.DoesExist ("WCS_torrus") Then
	WCS.reStore "WCS_torrus"
Else
	WCS.Store "WCS_torrus"
End If
WCS.restore "WCS_torrus"
 cst_torrus_r_torrus = Evaluate(scst_torrus_r_torrus)
 cst_torrus_r =  Evaluate(scst_torrus_r)
 cst_torrus_phi =  Evaluate(scst_torrus_phi)
 cst_torrus_g =  Evaluate(scst_torrus_g)
 cst_torrus_slope =  Evaluate(scst_torrus_slope)
 cst_torrus_N =  Evaluate(scst_torrus_N)
 cst_torrus_wire_d =  Evaluate(scst_torrus_wire_d)
 ' Begin construction
 'slope 
 If cst_torrus_slope_option = 1 Then 'height option
  cst_torrus_alpha = Atn(cst_torrus_slope/(2*pi*cst_torrus_r))*180/pi
 Else 'degree option
  cst_torrus_alpha = cst_torrus_slope 
 End If
 
 'check for penetration
 If cst_torrus_r_torrus + cst_torrus_wire_d > cst_torrus_r Then
  MsgBox "Wire radius +  Torrus Core-radius > Winding Core-Radius!"
  Exit All
 End If 
 'existing items?
 
 Curve.NewCurve "torrus_curve" 
 Curve.NewCurve "torrus_wire" 
 With Material
     .Reset 
     .Name "Ferrite"
     .FrqType "hf" 
     .Type "Normal" 
     .Epsilon "1.0" 
     .Mu "1.0" 
     .Kappa "0.0" 
     .TanD "0.0" 
     .TanDFreq "0.0" 
     .TanDGiven "False" 
     .TanDModel "ConstTanD" 
     .KappaM "0.0" 
     .TanDM "0.0" 
     .TanDMFreq "0.0" 
     .TanDMGiven "False" 
     .DispModelEps "None" 
     .DispModelMu "None" 
     .Rho "0.0" 
     .Colour "0.501961", "0.501961", "0.501961" 
     .Wireframe "False" 
     .Transparency "0" 
     .Create
 End With 
 With Material
     .Reset 
     .Name "Wire_material"
     .FrqType "hf" 
     .Type "Pec" 
     .Rho "0.0" 
     .Colour "1", "0.501961", "0.752941" 
     .Wireframe "False" 
     .Transparency "0" 
     .Create
 End With 
 With Polygon3D 
     .Reset 
     .Name "torrus_3dpolygon" 
     .Curve "torrus_curve" 
     For cst_torrus_i = 0 To cst_torrus_N*(360/cst_torrus_phi)
      'x = r*Cos(phi*i*pi/180)
      cst_torrus_y = cst_torrus_r*Sin(cst_torrus_phi*cst_torrus_i*pi/180)
      cst_torrus_h = cst_torrus_r*cst_torrus_phi*cst_torrus_i*pi/180*Tan(cst_torrus_alpha*pi/180) 
      cst_torrus_psi = cst_torrus_h/(cst_torrus_g-cst_torrus_r*Sin(cst_torrus_phi*pi/180))
     
      cst_torrus_x = cst_torrus_g-  _
         (cst_torrus_g-cst_torrus_r*Cos(cst_torrus_phi*cst_torrus_i*pi/180))*Cos(cst_torrus_psi)	'x-correction
      cst_torrus_h = (cst_torrus_g-cst_torrus_r*  _
          Cos(cst_torrus_phi*cst_torrus_i*pi/180))*Sin(cst_torrus_psi)     	'height- correction
      .Point cst_torrus_x, cst_torrus_y,cst_torrus_h 
     Next cst_torrus_i
     .Create 
 End With 
 With Cylinder 
     .Reset 
     .Name "dummy_zyl" 
     .Component "Ferrite" 
     .Material "Ferrite" 
     .OuterRadius cst_torrus_r_torrus
     .InnerRadius "0" 
     .Axis "z"
     .Zrange "0", "12" 
     .Xcenter "0" 
     .Ycenter "0" 
     .Create 
 End With 
 Pick.PickFaceFromId "Ferrite:dummy_zyl", "1" 
 Pick.AddEdge cst_torrus_g, "-1", "0", cst_torrus_g, "1", "0" 
 With Rotate 
     .Reset 
     .Name Solid.GetNextFreeName
     .Component "Ferrite" 
     .Material "Ferrite" 
     .Mode "Picks" 
     .Angle "360" 
     .Height "0.0" 
     .RadiusRatio "1.0" 
     .NSteps "0" 
     .Create 
 End With 
 Solid.Delete "Ferrite:dummy_zyl"
 'WCS.ActivateWCS "local"
 WCS.reStore "WCS_torrus"
 Pick.PickCurveEndpointFromId "torrus_curve:torrus_3dpolygon", "1"
 WCS.AlignWCSWithSelectedPoint
 'With WCS
     '.SetNormal "0.0", "0.0", "1.0"
     '.SetOrigin cst_torrus_r, "0.0", "0.0"
     '.SetUVector "1.0", "0.0", "0.0"
 'End With
 WCS.RotateWCS "u", 90+( cst_torrus_alpha) '*180/pi)	'rotate Coordinatesystem
 With Circle
     .Reset 
     .Name "wire" 
     .Curve "torrus_wire" 
     .Radius cst_torrus_wire_d 
     .Xcenter "0." 
     .Ycenter "0." 
     .Create
 End With
 With SweepCurve
     .Reset
     .Name Solid.GetNextFreeName
     .Component "Wire_material"
     .Material "Wire_material"
     .Twistangle "0.0"
     .Taperangle "0.0"
     .ProjectProfileToPathAdvanced "False"
     .Path "torrus_curve:torrus_3dpolygon"
     .Curve "torrus_wire:wire"
     .Create
End With
 Curve.DeleteCurve "torrus_curve"
 Curve.DeleteCurve "torrus_wire"
 
 WCS.ActivateWCS "global"
 Pick.ClearAllPicks

'@ define brick: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "0.2", "10.2" 
     .Yrange "-1,1", "-0,1" 
     .Zrange "-41", "-1" 
     .Create
End With

'@ rename block: component1:solid1 to: component1:GND

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:solid1", "GND"

'@ define brick: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "-11.2", "-1.2" 
     .Yrange "-1.1", "-0.1" 
     .Zrange "-41", "-1" 
     .Create
End With

'@ rename block: component1:solid1 to: component1:GND2

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:solid1", "GND2"

'@ define brick: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "-1.2", "0.2" 
     .Yrange "-1.1", "-0.1" 
     .Zrange "-41", "-5" 
     .Create
End With

'@ rename block: component1:solid1 to: component1:GND3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:solid1", "GND3"

'@ define brick: component1:solid1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Brick
     .Reset 
     .Name "solid1" 
     .Component "component1" 
     .Material "PEC" 
     .Xrange "4", "10,2" 
     .Yrange "-1,1", "-0,1" 
     .Zrange "-2", "20" 
     .Create
End With

'@ rename block: component1:solid1 to: component1:GND4

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:solid1", "GND4"

'@ boolean add shapes: component1:GND, component1:GND2

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:GND", "component1:GND2"

'@ boolean add shapes: component1:GND3, component1:GND4

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:GND3", "component1:GND4"

'@ boolean add shapes: component1:GND, component1:GND3

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:GND", "component1:GND3"

'@ boolean add shapes: component1:Feed, component1:Helix

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Add "component1:Feed", "component1:Helix"

'@ rename block: component1:Feed to: component1:Helix

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Solid.Rename "component1:Feed", "Helix"

'@ clear picks

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.ClearAllPicks

'@ pick circle center point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickCirclecenterFromId "component1:Helix", "4"

'@ pick mid point

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickMidpointFromId "component1:GND", "14"

'@ define discrete port: 1

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With DiscretePort 
     .Reset 
     .PortNumber "1" 
     .Type "SParameter" 
     .Label "Port1" 
     .Folder "Folder1" 
     .Impedance "50.0" 
     .VoltagePortImpedance "0.0" 
     .Voltage "1.0" 
     .Current "1.0" 
     .SetP1 "True", "-0.5", "-0.1", "-3" 
     .SetP2 "True", "-0.5", "-0.1", "-5" 
     .InvertDirection "False" 
     .LocalCoordinates "True" 
     .Monitor "True" 
     .Radius "0.0" 
     .Wire "" 
     .Position "end1" 
     .Create 
End With

'@ switch working plane

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawWorkplane "false"

'@ define farfield monitor: farfield (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "farfield (f=868)" 
     .Domain "Frequency" 
     .FieldType "Farfield" 
     .MonitorValue "868" 
     .ExportFarfieldSource "False" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "10", "10", "10", "10", "10", "10" 
     .SetSubvolumeInflateWithOffset "False" 
     .SetSubvolumeOffsetType "FractionOfWavelength" 
     .EnableNearfieldCalculation "True" 
     .Create 
End With

'@ define farfield monitor: farfield (f=864)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "farfield (f=864)" 
     .Domain "Frequency" 
     .FieldType "Farfield" 
     .MonitorValue "864" 
     .ExportFarfieldSource "False" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "10", "10", "10", "10", "10", "10" 
     .SetSubvolumeInflateWithOffset "False" 
     .SetSubvolumeOffsetType "FractionOfWavelength" 
     .EnableNearfieldCalculation "True" 
     .Create 
End With

'@ define farfield monitor: farfield (f=866)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "farfield (f=866)" 
     .Domain "Frequency" 
     .FieldType "Farfield" 
     .MonitorValue "866" 
     .ExportFarfieldSource "False" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "10", "10", "10", "10", "10", "10" 
     .SetSubvolumeInflateWithOffset "False" 
     .SetSubvolumeOffsetType "FractionOfWavelength" 
     .EnableNearfieldCalculation "True" 
     .Create 
End With

'@ define farfield monitor: farfield (f=869)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "farfield (f=869)" 
     .Domain "Frequency" 
     .FieldType "Farfield" 
     .MonitorValue "869" 
     .ExportFarfieldSource "False" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "10", "10", "10", "10", "10", "10" 
     .SetSubvolumeInflateWithOffset "False" 
     .SetSubvolumeOffsetType "FractionOfWavelength" 
     .EnableNearfieldCalculation "True" 
     .Create 
End With

'@ define monitor: e-field (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "e-field (f=868)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Efield" 
     .MonitorValue "868" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "0.0", "0.0", "0.0", "0.0", "0.0", "0.0" 
     .SetSubvolumeInflateWithOffset "False" 
     .Create 
End With

'@ define monitor: e-field (f=867)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "e-field (f=867)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Efield" 
     .MonitorValue "867" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "0.0", "0.0", "0.0", "0.0", "0.0", "0.0" 
     .SetSubvolumeInflateWithOffset "False" 
     .Create 
End With

'@ define monitor: e-field (f=866)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "e-field (f=866)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Efield" 
     .MonitorValue "866" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2919996723809", "3.2906231269025", "-41", "20" 
     .SetSubvolumeOffset "0.0", "0.0", "0.0", "0.0", "0.0", "0.0" 
     .SetSubvolumeInflateWithOffset "False" 
     .Create 
End With

'@ switch working plane

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawWorkplane "true"

'@ switch bounding box

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Plot.DrawBox "True"

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
     .SetFarfieldTransparent "True" 
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

'@ pick edge

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickEdgeFromId "component1:Helix", "2", "2"

'@ define distance dimension

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Dimension
    .Reset
    .CreationType "picks"
    .SetType "Distance"
    .SetID "0"
    .SetOrientation "Smart Mode"
    .SetDistance "5.189436"
    .SetViewVector "0.081260", "0.965295", "-0.248198"
    .SetConnectedElement1 "component1:Helix"
    .SetConnectedElement2 "component1:Helix"
    .Create
End With
Pick.ClearAllPicks

'@ clear picks

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.ClearAllPicks

'@ pick edge

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Pick.PickEdgeFromId "component1:Helix", "2", "2"

'@ define distance dimension

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Dimension
    .Reset
    .CreationType "picks"
    .SetType "Distance"
    .SetID "1"
    .SetOrientation "Smart Mode"
    .SetDistance "5.517282"
    .SetViewVector "0.001340", "0.999989", "0.004480"
    .SetConnectedElement1 "component1:Helix"
    .SetConnectedElement2 "component1:Helix"
    .Create
End With
Pick.ClearAllPicks

'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "3D" 
     .Vary "angle2" 
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

'@ delete monitors

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Monitor.Delete "e-field (f=866)" 
Monitor.Delete "e-field (f=867)"

'@ delete monitors

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Monitor.Delete "e-field (f=800)" 
Monitor.Delete "e-field (f=850)" 
Monitor.Delete "e-field (f=900)" 
Monitor.Delete "farfield (f=800)" 
Monitor.Delete "farfield (f=850)" 
Monitor.Delete "farfield (f=864)" 
Monitor.Delete "farfield (f=866)" 
Monitor.Delete "h-field (f=800)" 
Monitor.Delete "h-field (f=850)"

'@ delete monitors

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
Monitor.Delete "farfield (f=869)" 
Monitor.Delete "farfield (f=900)"

'@ delete monitor: h-field (f=900)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Delete "h-field (f=900)" 
End With

'@ define monitor: h-field (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "h-field (f=868)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Hfield" 
     .MonitorValue "868" 
     .UseSubvolume "False" 
     .Coordinates "Free" 
     .SetSubvolume "0.0", "0.0", "0.0", "0.0", "0.0", "0.0" 
     .SetSubvolumeOffset "0.0", "0.0", "0.0", "0.0", "0.0", "0.0" 
     .SetSubvolumeInflateWithOffset "False" 
     .Create 
End With

'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "Polar" 
     .Vary "angle1" 
     .Theta "0" 
     .Phi "0" 
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
     .SetAntennaType "isotropic" 
     .Phistart "1.000000e+00", "0.000000e+00", "0.000000e+00" 
     .Thetastart "0.000000e+00", "0.000000e+00", "1.000000e+00" 
     .PolarizationVector "0.000000e+00", "1.000000e+00", "0.000000e+00" 
     .SetCoordinateSystemType "spherical" 
     .SetAutomaticCoordinateSystem "True" 
     .SetPolarizationType "Abs" 
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
     .MaximumNumberOfGPUs "1"
End With
UseDistributedComputingForParameters "False"
MaxNumberOfDistributedComputingParameters "2"
UseDistributedComputingMemorySetting "False"
MinDistributedComputingMemoryLimit "0"
UseDistributedComputingSharedDirectory "False"

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
     .MaximumNumberOfGPUs "1"
End With
UseDistributedComputingForParameters "False"
MaxNumberOfDistributedComputingParameters "2"
UseDistributedComputingMemorySetting "False"
MinDistributedComputingMemoryLimit "0"
UseDistributedComputingSharedDirectory "False"

'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "3D" 
     .Vary "angle1" 
     .Theta "0" 
     .Phi "0" 
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
     .SetAntennaType "isotropic_linear" 
     .Phistart "1.000000e+00", "0.000000e+00", "0.000000e+00" 
     .Thetastart "0.000000e+00", "0.000000e+00", "1.000000e+00" 
     .PolarizationVector "0.000000e+00", "1.000000e+00", "0.000000e+00" 
     .SetCoordinateSystemType "ludwig2ae" 
     .SetAutomaticCoordinateSystem "True" 
     .SetPolarizationType "Slant" 
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

'@ define fieldsource monitor: field-source (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "field-source (f=868)" 
     .Domain "Frequency" 
     .FieldType "Fieldsource" 
     .Samples "1" 
     .MonitorValueRange "868", "868" 
     .InvertOrientation "False" 
     .UseSubvolume "False" 
     .Coordinates "Structure" 
     .SetSubvolume "-11.2", "10.2", "-3.2499998923898", "3.2499222341379", "-41", "20" 
     .SetSubvolumeOffset "10", "10", "10", "10", "10", "10" 
     .SetSubvolumeInflateWithOffset "False" 
     .SetSubvolumeOffsetType "FractionOfWavelength" 
     .Create 
End With

'@ define monitor: surface-current (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "surface-current (f=868)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Surfacecurrent" 
     .MonitorValue "868" 
     .Create 
End With

'@ define monitor: power (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "power (f=868)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Powerflow" 
     .MonitorValue "868" 
     .Create 
End With

'@ define monitor: current (f=868)

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With Monitor 
     .Reset 
     .Name "current (f=868)" 
     .Dimension "Volume" 
     .Domain "Frequency" 
     .FieldType "Current" 
     .MonitorValue "868" 
     .Create 
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
     .MaximumNumberOfGPUs "1"
End With
UseDistributedComputingForParameters "False"
MaxNumberOfDistributedComputingParameters "2"
UseDistributedComputingMemorySetting "False"
MinDistributedComputingMemoryLimit "0"
UseDistributedComputingSharedDirectory "False"

'@ farfield plot options

'[VERSION]2020.1|29.0.1|20191021[/VERSION]
With FarfieldPlot 
     .Plottype "Polar" 
     .Vary "angle1" 
     .Theta "0" 
     .Phi "0" 
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


