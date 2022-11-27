Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,0.0);
  var aab = point(0.0,-1.0);
  var aba = point(0.0,-1.0);
  var abb = point(1.0,-1.0);
  var aca = 0.8944271909999159e1+1.4472135954999579e2+0.5527864045000421e0;
  var acb = 1.8944271909999157e1+0.4472135954999579e2-0.4472135954999579e0;
  var acc = -0.5e1+0.5e2+0.5e0;
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
    aca, "aca",
    acb, "acb",
    acc, "acc",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
