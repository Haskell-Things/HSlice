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
  var aeb = 5.379988095711658e12-3.7174803446018445e02-4.352501798965642e01;
  var aec = -0.7071067811865478e1+0.7071067811865474e2+1.0606601717798212e0;
  var aed = -0.5257311121191337e1-0.8506508083520399e2-0.3249196962329061e0;
  var aeeaa = point(0.0,-1.0);
  var aeeab = point(1.0,-1.0);
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
    0x882288,
    [aeeaa,aeeab],
    0x00AA88,
    aeeaa, "aeeaa",
    aeeab, "aeeab",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
