Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(-1.0,1.0);
  var aab = point(-1.0,-1.0);
  var aba = point(-1.0,-1.0);
  var abb = point(0.0,0.0);
  var aca = point(0.0,0.0);
  var acb = point(1.0,-1.0);
  var ada = point(1.0,-1.0);
  var adb = point(1.0,1.0);
  var aea = point(1.0,1.0);
  var aeb = point(-1.0,1.0);
  var afa = -0.7071067811865475e1-0.7071067811865475e2+0.0e0;
  var afb = 0.9238795325112867e1-0.3826834323650897e2+0.541196100146197e0;
  var afc = 0.17157287525380988e1-0.4142135623730951e2+0.2426406871192852e0;
  var aga = 0.9238795325112867e1+0.3826834323650897e2-0.541196100146197e0;
  var agb = -0.7071067811865475e1+0.7071067811865475e2+0.0e0;
  var agc = 0.17157287525380982e1+0.41421356237309515e2-0.24264068711928521e0;
  var aha = 0.17157287525380988e1-0.4142135623730951e2+0.2426406871192852e0;
  var ahb = 1.414213562373095e1+0.0e2+0.0e0;
  var ahc = 0.17157287525380982e1+0.41421356237309515e2-0.24264068711928521e0;
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
    0x882288,
    [aea,aeb],
    0x00AA88,
    aea, "aea",
    aeb, "aeb",
    afa, "afa",
    afb, "afb",
    afc, "afc",
    aga, "aga",
    agb, "agb",
    agc, "agc",
    aha, "aha",
    ahb, "ahb",
    ahc, "ahc",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
