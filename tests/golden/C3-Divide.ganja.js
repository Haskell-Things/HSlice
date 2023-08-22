Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaaa = point(1.0,1.0);
  var aaab = point(0.0,0.0);
  var aaba = point(0.0,0.0);
  var aabb = point(-1.0,1.0);
  var aac = -1.0e1+0.0e2+0.0e0;
  var ab = 2.414213562373095e12+0.0e02-1.4142135623730951e01;
  var ac = -0.3826834323650897e1+0.9238795325112868e2+0.5411961001461972e0;
  var ad = -0.3826834323650897e1-0.9238795325112868e2-0.5411961001461972e0;
  var aeaa = point(-1.0,-1.0);
  var aeab = point(1.0,-1.0);
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
    0x882288,
    [aeaa,aeab],
    0x00AA88,
    aeaa, "aeaa",
    aeab, "aeab",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
