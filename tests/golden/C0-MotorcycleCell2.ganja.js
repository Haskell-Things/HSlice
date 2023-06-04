Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,0.0);
  var aab = point(-1.0,-1.0);
  var aba = point(-1.0,-1.0);
  var abb = point(1.0,-1.0);
  var aca = point(1.0,-1.0);
  var acb = point(1.0,1.0);
  var adaa = point(-1.0,1.0);
  var adab = point(0.0,0.0);
  var adba = point(0.0,0.0);
  var adbb = point(-1.0,-1.0);
  var adc = 0.0e1-1.0e2+0.0e0;
  var aeaa = point(0.0,0.0);
  var aeab = point(-1.0,-1.0);
  var aeba = point(-1.0,-1.0);
  var aebb = point(1.0,-1.0);
  var aeca = point(1.0,-1.0);
  var aecb = point(1.0,1.0);
  var aeda = 0.3826834323650897e1-0.9238795325112867e2-0.541196100146197e0;
  var aedb = 0.7071067811865475e1+0.7071067811865475e2+0.0e0;
  var aedc = 0.9807852804032305e1-0.19509032201612836e2-0.4870636221857319e0;
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
    [adaa,adab],
    0x00AA88,
    adaa, "adaa",
    adab, "adab",
    0x882288,
    [adba,adbb],
    0x00AA88,
    adba, "adba",
    adbb, "adbb",
    adc, "adc",
    0x882288,
    [aeaa,aeab],
    0x00AA88,
    aeaa, "aeaa",
    aeab, "aeab",
    0x882288,
    [aeba,aebb],
    0x00AA88,
    aeba, "aeba",
    aebb, "aebb",
    0x882288,
    [aeca,aecb],
    0x00AA88,
    aeca, "aeca",
    aecb, "aecb",
    aeda, "aeda",
    aedb, "aedb",
    aedc, "aedc",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
