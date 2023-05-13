Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaaa = point(-1.0,1.0);
  var aaab = point(0.0,0.0);
  var aaba = point(0.0,0.0);
  var aabb = point(-1.0,-1.0);
  var aac = 0.0e1-1.0e2+0.0e0;
  var ab = 2.8284271247461903e12-2.8284271247461903e02+0.0e01;
  var ac = 0.7071067811865476e1+0.7071067811865476e2-0.7071067811865476e0;
  var ad = 0.7071067811865476e1-0.7071067811865476e2-0.7071067811865476e0;
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
