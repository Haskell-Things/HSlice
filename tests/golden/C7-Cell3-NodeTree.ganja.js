Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.5,0.0);
  var aab = point(0.0,1.0);
  var aba = point(0.0,1.0);
  var abb = point(-1.0,1.0);
  var aca = point(-1.0,1.0);
  var acb = point(-1.0,0.0);
  var ada = point(-1.0,0.0);
  var adb = point(0.0,0.0);
  var aea = -0.5e1-0.5e2+0.0e0;
  var aeb = 0.5e1-0.5e2+0.5e0;
  var aec = 0.0e1-1.414213562373095e2+0.7071067811865475e0;
  var afa = -0.8944271909999159e1+0.5527864045000421e2-0.5527864045000421e0;
  var afb = 0.0e1-1.414213562373095e2+0.7071067811865475e0;
  var afc = -0.8506508083520399e1-0.4742688878808663e2-0.025731112119133703e0;
  document.body.appendChild(this.graph([
    0x882288,
    [aaa,aab],
    0x00AA88,
    aaa, "aaa",
    aab, "aab",
    0x882288,
    [aba,abb],
    0x00AA88,
    aba, "aba",
    abb, "abb",
    0x882288,
    [aca,acb],
    0x00AA88,
    aca, "aca",
    acb, "acb",
    0x882288,
    [ada,adb],
    0x00AA88,
    ada, "ada",
    adb, "adb",
    aea, "aea",
    aeb, "aeb",
    aec, "aec",
    afa, "afa",
    afb, "afb",
    afc, "afc",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
