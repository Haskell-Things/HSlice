Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,-1.0);
  var aab = point(1.0,-1.0);
  var aba = point(1.0,-1.0);
  var abb = point(1.0,1.0);
  var aca = point(1.0,1.0);
  var acb = point(0.5,1.0);
  var ada = point(0.5,1.0);
  var adb = point(0.5,0.0);
  var aeaaa = point(0.5,1.0);
  var aeaab = point(0.5,0.0);
  var aeaba = point(0.5,0.0);
  var aeabb = point(0.0,1.0);
  var aeac = -0.9732489894677302e1-0.22975292054736118e2+0.4866244947338651e0;
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
    [aeaaa,aeaab],
    0x00AA88,
    aeaaa, "aeaaa",
    aeaab, "aeaab",
    0x882288,
    [aeaba,aeabb],
    0x00AA88,
    aeaba, "aeaba",
    aeabb, "aeabb",
    aeac, "aeac",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
