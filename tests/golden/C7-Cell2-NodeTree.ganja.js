Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,0.0);
  var aab = point(0.0,-1.0);
  var aba = point(0.0,-1.0);
  var abb = point(1.0,-1.0);
  var aca = 0.5547001962252291e1+0.8320502943378437e2+0.2773500981126146e0;
  var acb = 0.9701425001453319e1+0.24253562503633297e2-0.24253562503633297e0;
  var acc = -0.7071067811865476e1+0.7071067811865476e2+0.7071067811865476e0;
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
