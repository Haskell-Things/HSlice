Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.5,0.0);
  var aab = point(0.0,1.0);
  var aba = point(0.0,0.0);
  var abb = point(0.0,-1.0);
  var aca = point(0.0,-1.0);
  var acb = point(1.0,-1.0);
  var adaaa = point(-1.0,0.0);
  var adaab = point(0.0,0.0);
  var adaba = point(0.0,0.0);
  var adabb = point(0.0,-1.0);
  var adac = 0.7071067811865475e1-0.7071067811865475e2+0.0e0;
  var adb = 2.468306115762555e12-0.4714045207910317e02+0.4714045207910317e01;
  var adc = -0.5257311121191335e1-0.85065080835204e2+0.26286555605956674e0;
  var add = 0.9732489894677302e1+0.22975292054736116e2-0.22975292054736116e0;
  var adeaa = point(0.5,0.0);
  var adeab = point(0.0,1.0);
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
    [adaaa,adaab],
    0x00AA88,
    adaaa, "adaaa",
    adaab, "adaab",
    0x882288,
    [adaba,adabb],
    0x00AA88,
    adaba, "adaba",
    adabb, "adabb",
    adac, "adac",
    adb, "adb",
    adc, "adc",
    add, "add",
    0x882288,
    [adeaa,adeab],
    0x00AA88,
    adeaa, "adeaa",
    adeab, "adeab",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
