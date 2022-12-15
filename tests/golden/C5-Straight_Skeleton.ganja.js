Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,0.0);
  var aab = point(-1.0,-1.0);
  var aba = point(-1.0,-1.0);
  var abb = point(1.0,-1.0);
  var aca = point(1.0,-1.0);
  var acb = point(2.0,0.0);
  var ada = point(2.0,0.0);
  var adb = point(1.0,1.0);
  var aea = point(1.0,1.0);
  var aeb = point(-1.0,1.0);
  var afa = point(-1.0,1.0);
  var afb = point(0.0,0.0);
  var aga = 0.7071067811865475e1-1.7071067811865475e2-1.0e0;
  var agb = 0.7071067811865475e1+0.29289321881345254e2-0.4142135623730949e0;
  var agc = 0.2928932188134526e1-0.29289321881345265e2-0.2928932188134526e0;
  var aha = -0.7071067811865475e1+0.29289321881345254e2+0.4142135623730949e0;
  var ahb = -0.7071067811865475e1-1.7071067811865475e2+1.0e0;
  var ahc = -0.2928932188134524e1-0.29289321881345254e2+0.2928932188134524e0;
  var aia = 0.0e1-1.414213562373095e2+0.0e0;
  var aib = 0.2928932188134526e1-0.29289321881345265e2-0.2928932188134526e0;
  var aic = 0.0e1+1.0e2+0.0e0;
  var aid = -0.2928932188134524e1-0.29289321881345254e2+0.2928932188134524e0;
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
    0x882288,
    [afa,afb],
    0x00AA88,
    afa, "afa",
    afb, "afb",
    aga, "aga",
    agb, "agb",
    agc, "agc",
    aha, "aha",
    ahb, "ahb",
    ahc, "ahc",
    aia, "aia",
    aib, "aib",
    aic, "aic",
    aid, "aid",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
