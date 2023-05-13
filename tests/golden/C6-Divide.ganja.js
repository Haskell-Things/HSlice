Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaaa = point(-0.5,-1.0);
  var aaab = point(0.0,0.0);
  var aaba = point(0.0,0.0);
  var aabb = point(0.5,-1.0);
  var aac = 1.0e1+0.0e2+0.0e0;
  var ab = 3.23606797749979e12+0.0e02+2.23606797749979e01;
  var ac = 0.5257311121191336e1-0.8506508083520399e2+0.5877852522924731e0;
  var ad = 0.5257311121191336e1+0.8506508083520399e2-0.5877852522924731e0;
  document.body.appendChild(this.graph([
    0x882288,
    [aaaa,aaab],
    0x00AA88,
    aaaa, "aaaa",
    aaab, "aaab",
    0x882288,
    [aaba,aabb],
    0x00AA88,
    aaba, "aaba",
    aabb, "aabb",
    aac, "aac",
    ab, "ab",
    ac, "ac",
    ad, "ad",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
