Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(-1.0,-1.0);
  var aab = point(1.0,-1.0);
  var aba = point(1.0,-1.0);
  var abb = point(1.0,1.0);
  var aca = point(1.0,1.0);
  var acb = point(-1.0,1.0);
  var ada = point(-1.0,1.0);
  var adb = point(-1.0,-1.0);
  var aea = 0.7071067811865475e1-0.7071067811865475e2+0.0e0;
  var aeb = 0.7071067811865475e1+0.7071067811865475e2+0.0e0;
  var aec = -0.7071067811865475e1+0.7071067811865475e2+0.0e0;
  var aed = -0.7071067811865475e1-0.7071067811865475e2+0.0e0;
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
