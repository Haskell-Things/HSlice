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
  var aeaaa = point(-1.0,0.0);
  var aeaab = point(0.0,0.0);
  var aeaba = point(0.0,0.0);
  var aeabb = point(0.0,-1.0);
  var aeac = 0.7071067811865475e1-0.7071067811865475e2+0.0e0;
  var aeb = 2.468306115762555e12-0.4714045207910317e02+0.4714045207910317e01;
  var aec = -0.5257311121191335e1-0.85065080835204e2+0.26286555605956674e0;
  var aed = 0.9732489894677302e1+0.22975292054736116e2-0.22975292054736116e0;
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
    aeb, "aeb",
    aec, "aec",
    aed, "aed",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
