Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaaa = point(0.5,1.0);
  var aaab = point(0.5,0.0);
  var aaba = point(0.5,0.0);
  var aabb = point(0.0,1.0);
  var aac = -0.9732489894677302e1-0.22975292054736118e2+0.4866244947338651e0;
  var ab = 5.379988095711658e12-3.7174803446018445e02-4.352501798965642e01;
  var ac = -0.7071067811865478e1+0.7071067811865474e2+1.0606601717798212e0;
  var ad = -0.5257311121191337e1-0.8506508083520399e2-0.3249196962329061e0;
  var aeaa = point(0.0,-1.0);
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
