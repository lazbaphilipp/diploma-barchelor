<?xml version="1.0" encoding="UTF-8"?>
<MetaResultFile creator="Solver HFTD - Field 3DFD Monitor" version="20170330">
  <SpecialMaterials>
    <Background type="NORMAL"/>
    <Material name="PEC" type="FIELDFREE"/>
  </SpecialMaterials>
  <SimulationProperties dB_Amplitude="10" encoded_unit="&amp;U:V^1.:A^1.:m^-2" fieldname="power (f=868) [1]" fieldtype="Powerflow" frequency="868"/>
  <MetaGeometryFile filename="model.gex" lod="1"/>
  <ResultGroups num_steps="1" transformation="1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1">
    <Frame index="0">
      <FieldResultFile filename="power (f=868)_1,1.m3d" type="m3d"/>
    </Frame>
  </ResultGroups>
  <ResultDataType complex="0" timedomain="0" vector="1"/>
  <SimulationDomain min="-99.37425231934 -91.42425537109 -129.1742553711" max="98.37425231934 91.42417144775 108.1742553711"/>
  <PlotSettings Plot="4" ignore_symmetry="0" deformation="0"/>
  <Source type="SOLVER"/>
</MetaResultFile>
