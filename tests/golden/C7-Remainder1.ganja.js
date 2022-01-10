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
  var aea = point(0.0,0.0);
  var aeb = point(0.0,-1.0);
  var afa = point(0.0,-1.0);
  var afb = point(1.0,-1.0);
  var agaaa = point(-1.0,0.0);
  var agaab = point(0.0,0.0);
  var agaba = point(0.0,0.0);
  var agabb = point(0.0,-1.0);
  var agac = 1.0e1-1.0e2+0.0e0;
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
    0x882288,
    [agaaa,agaab],
    0x00AA88,
    agaaa, "agaaa",
    agaab, "agaab",
    0x882288,
    [agaba,agabb],
    0x00AA88,
    agaba, "agaba",
    agabb, "agabb",
    agac, "agac",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
