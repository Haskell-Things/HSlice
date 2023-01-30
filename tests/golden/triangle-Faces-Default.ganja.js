Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(0.0,0.0);
  var aab = point(2.0,0.0);
  var aba = point(2.0,0.0);
  var abb = point(1.0,1.7320508075688772);
  var aca = point(1.0,1.7320508075688772);
  var acb = point(0.0,0.0);
  var ad = 0.5000000000000001e1+0.8660254037844387e2-1.0000000000000002e0;
  var ae = 0.5000000000000001e1-0.8660254037844387e2+0.0e0;
  var af = -1.0e1+0.0e2+1.0e0;
  var ag = 0.5000000000000001e1+0.8660254037844387e2-1.0000000000000002e0;
  var ah = 0.5000000000000001e1-0.8660254037844387e2+0.0e0;
  var ai = -1.0e1+0.0e2+1.0e0;
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
    ad, "ad",
    ae, "ae",
    af, "af",
    ag, "ag",
    ah, "ah",
    ai, "ai",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
