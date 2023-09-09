Algebra(2,0,1,()=>{
  var line = (a,b,c)=>a*1e1 + b*1e2 + c*1e0;
  var point = (x,y)=>!(1e0 + x*1e1 + y*1e2);
  var aaa = point(1.0,1.7320508075688772);
  var aab = point(0.0,0.0);
  var aba = point(0.0,0.0);
  var abb = point(2.0,0.0);
  var aca = point(2.0,0.0);
  var acb = point(1.0,1.7320508075688772);
  var ada = -1.0e1+0.0e2+1.0e0;
  var adb = 0.5000000000000001e1-0.8660254037844387e2+0.0e0;
  var adc = 0.5000000000000001e1+0.8660254037844387e2-1.0000000000000002e0;
  var ba = point(1.826794919243112,0.1);
  var bb = point(1.0,1.5320508075688768);
  var bc = point(0.17320508075688773,0.1);
  var ca = point(1.6535898384862244,0.2);
  var cb = point(1.0000000000000002,1.3320508075688773);
  var cc = point(0.34641016151377546,0.2);
  var da = point(1.4803847577293368,0.3);
  var db = point(1.0,1.132050807568877);
  var dc = point(0.5196152422706632,0.3);
  var ea = point(1.3071796769724493,0.4);
  var eb = point(1.0,0.9320508075688773);
  var ec = point(0.6928203230275509,0.4);
  var fa = point(1.1339745962155614,0.5);
  var fb = point(1.0,0.7320508075688773);
  var fc = point(0.8660254037844386,0.5);
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
    ada, "ada",
    adb, "adb",
    adc, "adc",
    0x882288,
    [ba,bb],
    [bb,bc],
    [bc,ba],
    0x00AA88,
    ba, "ba",
    bb, "bb",
    bc, "bc",
    0x882288,
    [ca,cb],
    [cb,cc],
    [cc,ca],
    0x00AA88,
    ca, "ca",
    cb, "cb",
    cc, "cc",
    0x882288,
    [da,db],
    [db,dc],
    [dc,da],
    0x00AA88,
    da, "da",
    db, "db",
    dc, "dc",
    0x882288,
    [ea,eb],
    [eb,ec],
    [ec,ea],
    0x00AA88,
    ea, "ea",
    eb, "eb",
    ec, "ec",
    0x882288,
    [fa,fb],
    [fb,fc],
    [fc,fa],
    0x00AA88,
    fa, "fa",
    fb, "fb",
    fc, "fc",
  ],{
    grid: true,
    labels: true,
    lineWidth: 3,
    pointRadius: 1,
    fontSize: 1,
    scale: 1,
}));
});
