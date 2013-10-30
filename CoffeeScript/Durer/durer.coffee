# -----------------------------------------------------------------------------
#  Copyright 2013 Paul C. Roberts
#
#  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file 
#  except in compliance with the License. You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software distributed under the 
#  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
#  either express or implied. See the License for the specific language governing permissions and 
#  limitations under the License.
# -----------------------------------------------------------------------------

# nobare

showProofsChanged = () ->
  checkbox = document.getElementById "showProofs"
  proofs = document.getElementById "proofs"

  if checkbox.checked
    proofs.style.display = "block"
  else
    proofs.style.display = "none"

  return

showSampleChanged = () ->
  checkbox = document.getElementById 'showSample'
  sample = document.getElementById 'sample'

  if checkbox.checked
    sample.style.display = 'block'
  else
    sample.style.display = 'none'

  return

redrawProofs = () ->
  sys.showOutlines = document.getElementById('showOutlines').checked
  sys.overshoot = document.getElementById('overshoot').checked
  sys.fillGlyph = document.getElementById('fillGlyph').checked
  sys.glyphColor = document.getElementById('glyphColor').value
  sys.strokeGlyph = document.getElementById('strokeGlyph').checked

  drawProofs()
  return

changeProofSize = () ->
  size = (document.getElementById 'proofSize').value | 0

  sheet = document.styleSheets[0]
  if sheet.cssRules?
    canvasRule = sheet.cssRules[1]
    wideCanvasRule = sheet.cssRules[2]
  else if sheet.rules?
    canvasRule = sheet.rules[1]
    wideCanvasRule = sheet.rules[2]
  else
    console.log 'cannot change stylesheet'
    return

  canvasRule.style.width = size + 'px'
  canvasRule.style.height = size + 'px'
  wideCanvasRule.style.width = (2 * size) + 'px'
  return

changeProportions = () ->
  proportions.readFromDocument()
  drawProofs()
  drawSampleText()
  return

changeSampleText = () ->
  drawSampleText()
  return

sys = 
  canvas: null
  ctx: null

  width:0
  height:0
  x_margin:0
  y_margin:0

  working_width:0
  working_height:0

  overshoot: true
  showOutlines: true
  fillGlyph: true
  strokeGlyph: false
  glyphColor: 'black'

class Point
  constructor: (@x, @y) ->
    @isCanvas = false

  toCanvas: () ->
    if @isCanvas
      throw 'Already a canvas point'

    cx = @x * sys.working_width + sys.x_margin
    cy = @y * sys.working_height + sys.y_margin

    cpt = new Point cx, cy
    cpt.isCanvas = true
    return cpt

  fromCanvas: () ->
    if not @isCanvas
      throw 'This is not a canvas point'

    ax = (@x - sys.x_margin) / sys.working_width
    ay = (@y - sys.y_margin) / sys.working_height

    new Point ax, ay

  towards: (other, dist) ->
    dx = other.x - @x
    dy = other.y - @y
    len = Math.sqrt dx * dx + dy * dy

    new Point @x + (dx * dist) / len, @y + (dy * dist) / len

  distance: (other) ->
    dx = @x - other.x
    dy = @y - other.y
    Math.sqrt dx * dx + dy * dy

intersect = (a, b, c, d) ->
  slope_a = if a.x != b.x then (b.y - a.y) / (b.x - a.x) else 'vertical'
  slope_c = if c.x != d.x then (d.y - c.y) / (d.x - c.x) else 'vertical'

  if slope_a == slope_c
    return undefined
  else if slope_a == 'vertical'
    return new Point a.x, slope_c * (a.x - c.x) + c.y
  else if slope_c == 'vertical'
    return new Point c.x, slope_a * (c.x - a.x) + a.y

  # equations are y = slope_a (x - a.x) + a.y
  #               y = slope_c (x - c.x) + c.y
  ac = a.y - slope_a * a.x
  cc = c.y - slope_c * c.x

  x = (cc - ac) / (slope_a - slope_c)
  y = slope_a * x + ac

  new Point x, y

vertCircle = (c, r, x) ->
  dx = x - c.x
  if Math.abs(dx) > r
    return undefined

  dy = Math.sqrt r * r - dx * dx

  {
    upper: new Point x, c.y - dy
    lower: new Point x, c.y + dy
  }

intersectCircles = (ac, ar, bc, br) ->
  d = ac.distance bc
  d1 = (d * d - br * br + ar * ar) / (2 * d)

  a = Math.sqrt ar * ar - d1 * d1
  pd = ac.towards bc, d1
  dx = pd.x - ac.x
  dy = pd.y - ac.y

  dir = new Point pd.x + dy, pd.y - dx

  {
    left: pd.towards dir, a
    right: pd.towards dir, -a
  }

partWay = (left, right, prop) ->
  iprop = 1 - prop

  new Point left.x * iprop + right.x * prop, left.y * iprop + right.y * prop

midPoint = (left, right) ->
  new Point (left.x + right.x) / 2, (left.y + right.y) / 2

class OutlineDrawer
  constructor: (@context) ->

  drawBezier: (from, cp1, cp2, to) ->
    if not sys.showOutlines
      return
    @context.beginPath()
    @context.moveTo from.x, from.y
    @context.bezierCurveTo cp1.x, cp1.y, cp2.x, cp2.y, to.x, to.y
    @context.stroke()

  drawLine: (from, to) ->
    if not sys.showOutlines
      return
    if sys.overshoot
      fpt = partWay(from, to, -0.1).toCanvas()
      tpt = partWay(to, from, -0.1).toCanvas()
    else
      fpt = from.toCanvas()
      tpt = to.toCanvas()

    @context.beginPath()
    @context.moveTo fpt.x, fpt.y
    @context.lineTo tpt.x, tpt.y
    @context.stroke()

  drawCircle: (c, p) ->
    if not sys.showOutlines
      return
    c = c.toCanvas()
    p = p.toCanvas()

    radius = c.distance p

    @context.beginPath()
    @context.arc c.x, c.y, radius, 0, Math.PI * 2, true
    @context.stroke()

  drawTouchingCircle: (a, b, c) ->
    dab = a.distance b
    dac = b.distance c

    if dab < dac
      c = b.towards c, dab
    else
      a = b.towards a, dac

    adx = a.x - b.x
    ady = a.y - b.y

    cdx = c.x - b.x
    cdy = c.y - b.y

    cc = intersect a,
      new Point a.x - ady, a.y + adx
      c
      new Point c.x + cdy, c.y - cdx

    @drawLine c, cc
    @drawLine a, cc
    @drawCircle cc, a

    {
      from: a
      to: c
      center:cc
    }

  labelPoints: (named_points) ->
    if not sys.showOutlines
        return
    @context.font = '15px Times New Roman'
    @context.textAlign = 'center'
    @context.textBaseline = 'middle'
    @context.fillStyle = 'black'
    c = new Point(0.5, 0.5).toCanvas()
    lr = new Point(1, 1).toCanvas()

    for n, v of named_points
      pt = v.toCanvas()
      if 10 >= c.distance pt
        ptc = pt.towards lr, -10
      else
        ptc = pt.towards c, -10

      @context.fillText n, ptc.x, ptc.y

    return

class SvgPathGenerator
  constructor: () ->
    @path = ''
    @hasLimits = false
    @kernBlocks = 5
    @leftLimits = []
    @rightLimits = []

    for i in [0...@kernBlocks]
      @leftLimits[i] = 1
      @rightLimits[i] = 0

  append: (item) ->
    if @path != ''
      @path += ' '

    @path += item
    return

  computeKernLimits: (x, y) ->
    if @last_y < y
      ax = @last_x
      ay = @last_y
      cx = x
      cy = y
    else
      ax = x
      ay = y
      cx = @last_x
      cy = @last_y

    m = if ax != cx then (cy - ay) / (cx - ax) else 'vertical'

    c_y = 0
    for i in [0...@kernBlocks]
      t_y = c_y
      c_y = (i + 1) / @kernBlocks

      if (ay > c_y) or (cy < t_y)
        continue

      if m == 'vertical'
        c_x = t_x = ax
      else if m == 0 # horizontal
        if ay >= t_y and ay <= c_y
          @leftLimits[i] = Math.min @leftLimits[i], ax, cx
          @rightLimits[i] = Math.max @rightLimits[i], ax, cx
          return
        continue
      else
        t_x = ax + (t_y - ay) / m
        c_x = ax + (c_y - ay) / m

      if ay > t_y
        t_x = ax
      if cy < c_y
        c_x = cx

      @leftLimits[i] = Math.min @leftLimits[i], t_x, c_x
      @rightLimits[i] = Math.max @rightLimits[i], t_x, c_x

    return

  extendLimits: (x, y) ->
    if not @hasLimits
     @lx = @rx = x
     @ty = @by = y
     @hasLimits = true
    else
      if x < @lx
        @lx = x
      else if x > @rx
        @rx = x
      if y < @ty
        @ty = y
      else if y > @by
        @by = y

    @computeKernLimits x, y
    @last_x = x
    @last_y = y
    return

  beginPath: () ->
    @path = ''
    @hasLimits = false
    return

  moveTo: (x, y) ->
    @last_x = x
    @last_y = y
    @append 'M ' + x + ' ' + y
    return

  lineTo: (x, y) ->
    @extendLimits x, y
    @append 'L ' + x + ' ' + y
    return

  two_pi = Math.PI * 2
  pi_by_two = Math.PI / 2

  clampAngle: (a) ->
    a = parseFloat a

    while a < 0
      a += two_pi
    while a >= two_pi
      a -= two_pi

    return a

  arc: (cx, cy, radius, from, to, forwards) ->
    from = @clampAngle from
    to = @clampAngle to

    if forwards 
      while to > from
        to -= two_pi
    else
      while to < from
        to += two_pi

    ctr = Math.cos from
    str = Math.sin from
    ex = cx + radius * ctr
    ey = cy + radius * str
    f = 0.551784 * radius

    @lineTo ex, ey

    while (forwards and (to < from)) or (not forwards and (to > from))
      if Math.abs(to - from) > pi_by_two
        partial = if forwards then from - pi_by_two else from + pi_by_two
      else
        partial = to

      cfr = ctr
      sfr = str
      sx = ex
      sy = ey

      ctr = Math.cos partial
      str = Math.sin partial
      ex = cx + radius * ctr
      ey = cy + radius * str

      cpf = f * (partial - from) / pi_by_two

      @bezierCurveTo(
          sx - cpf * sfr, sy + cpf * cfr, 
          ex + cpf * str, ey - cpf * ctr, 
          ex, ey)
      from = partial
    return

  bezierCurveTo: (cp1x, cp1y, cp2x, cp2y, tx, ty) ->
    @extendLimits cp1x, cp1y
    @extendLimits cp2x, cp2y
    @extendLimits tx, ty
    @append 'C ' + cp1x + ' ' + cp1y + ' ' + cp2x + ' ' + cp2y + ' ' + tx + ' ' + ty
    return

  closePath: () ->
    @append 'Z'
    return

  fill: () ->
    return

  stroke: () ->
    return

class FillShape
  constructor: (@context) ->
    @context.beginPath()

  moveTo: (p) ->
    p = p.toCanvas()
    @context.moveTo p.x, p.y
    return

  lineTo: (p) ->
    p = p.toCanvas()
    @context.lineTo p.x, p.y
    return

  addArc: (a, forwards) ->
    from = a.from.toCanvas()
    to = a.to.toCanvas()
    center = a.center.toCanvas()

    radius = center.distance from

    start = Math.atan2 from.y - center.y, from.x - center.x
    end = Math.atan2 to.y - center.y, to.x - center.x

    @context.arc center.x, center.y, radius, start, end, forwards
    return

  addBezier: (cp1, cp2, to) ->
    cp1 = cp1.toCanvas()
    cp2 = cp2.toCanvas()
    to = to.toCanvas()

    @context.bezierCurveTo cp1.x, cp1.y, cp2.x, cp2.y, to.x, to.y
    return

  closeAndFill: () ->
    @context.closePath()
    if sys.fillGlyph
      @context.fillStyle = sys.glyphColor
      @context.fill()
    if sys.strokeGlyph
      if sys.fillGlyph and sys.glyphColor == 'white'
        @context.strokeStyle = 'black'
      else
        @context.strokeStyle = sys.glyphColor
      @context.lineWidth = 1
      @context.stroke()
    return

Util =
  initializeCanvas: (id) ->
    if id == undefined
      sys.ctx = new SvgPathGenerator()
      sys.width = 1
      sys.height = 1
      sys.x_margin = 0
      sys.y_margin = 0
      sys.working_width = 1
      sys.working_height = 1
      return

    sys.canvas = document.getElementById id
    sys.ctx = sys.canvas.getContext '2d'

    sys.ctx.lineWidth = 0.25
    sys.ctx.strokeStyle = 'black'
    sys.ctx.fillStyle = 'black'

    sys.width = sys.canvas.width
    sys.height = sys.canvas.height
    sys.ctx.clearRect 0, 0, sys.width, sys.height
    if sys.width > sys.height
      sys.width = sys.height
    else
      sys.height = sys.width

    sys.x_margin = sys.width * 0.1
    sys.y_margin = sys.height * 0.1

    sys.working_width = sys.width - 2 * sys.x_margin
    sys.working_height = sys.height - 2 * sys.y_margin
    return

  initializeSquare: (named_points, aspect) ->
    if aspect <= 1
      top = 0
      bottom = 1
      left = 0.5 - aspect / 2
      right = 0.5 + aspect / 2
    else
      left = 0
      right = 1
      top = 1 - 1/aspect
      bottom = 1

    named_points.a = new Point left, top
    named_points.b = new Point right, top
    named_points.c = new Point left, bottom
    named_points.d = new Point right, bottom

    right - left

GlyphFunctions = 
  drawA: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    if variant != 'b' and variant != 'c'
        variant = 'a'

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, b
    f = named_points.f = midPoint c, d
    outline.drawLine e, f

    g = named_points.g = midPoint a, c
    h = named_points.h = midPoint b, d
    outline.drawLine g, h

    i = named_points.i = c.towards d, serif
    k = named_points.k = d.towards c, serif

    eleft = e.towards a, narrow
    eright = eleft.towards b, wide

    if variant == 'b'
        eleft = e.towards a, wide
        eright = e.towards b, narrow

    if variant == 'a'
      outline.drawLine i, eleft
      outline.drawLine k, eright
    else if variant == 'b'
      outline.drawLine i, e
      outline.drawLine e, k
    else if variant == 'c'
      outline.drawLine i, eleft
      outline.drawLine k, eright

    iright = i.towards d, narrow
    kleft = k.towards c, wide

    if variant == 'a'
      outline.drawLine iright, e
      outline.drawLine kleft, eleft
    else if variant == 'b'
      outline.drawLine iright, eright
      outline.drawLine eleft, kleft
    else if variant == 'c'
      outline.drawLine iright, e
      outline.drawLine kleft, eleft

    gdown = g.towards c, narrow
    hdown = h.towards d, narrow
    outline.drawLine gdown, hdown

    isl = outline.drawTouchingCircle c, i, eleft
    isr = outline.drawTouchingCircle e, iright, iright.towards(d, serif)

    ksr = outline.drawTouchingCircle eright, k, d
    ksl = outline.drawTouchingCircle kleft.towards(c, serif), kleft, eleft

    if variant == 'a'
      ert = eright.towards k, serif / 2
      outline.drawLine eright, ert
      dx = ert.x - eright.x
      dy = ert.y - eright.y
      tcc = new Point eright.x + dy, eright.y - dx
      outline.drawLine eright, tcc
      outline.drawCircle tcc, eright
      tcct = new Point tcc.x - dx, tcc.y - dy
      outline.drawLine tcc, tcct

      outline.drawBezier tcct, tcct.towards(a, serif / 2), eleft.towards(k, serif / 2), eleft
    else if variant == 'c'
        esl = outline.drawTouchingCircle i, eleft, eleft.towards(a, serif)

    render = new FillShape sys.ctx

    render.moveTo c
    render.addArc isl, true
    if variant == 'a'
      render.lineTo eleft
      render.addBezier eleft.towards(k, serif / 2), tcct.towards(a, serif / 2), tcct
      render.addArc {from:tcct, to:eright, center:tcc}, true
    else if variant == 'b'
        render.lineTo e
    else if variant == 'c'
      render.lineTo esl.from
      render.addArc esl, true
      render.lineTo eright

    render.lineTo ksr.from
    render.addArc ksr, true
    render.lineTo kleft
    render.addArc ksl, true
    render.lineTo intersect(kleft, eleft, gdown, hdown)
    if variant == 'a' or variant == 'c'
      render.lineTo intersect(iright, e, gdown, hdown)
    else if variant == 'b'
      render.lineTo intersect(iright, eright, gdown, hdown)

    render.lineTo isr.from
    render.addArc isr, true
    render.lineTo c

    if variant == 'a' or variant == 'c'
      render.moveTo intersect(g, h, iright, e)
      render.lineTo intersect(kleft, eleft, g, h)
      render.lineTo intersect(iright, e, kleft, eleft)
    else if variant == 'b'
      render.moveTo intersect(g, h, iright, eright)
      render.lineTo intersect(kleft, eleft, g, h)
      render.lineTo intersect(iright, eright, kleft, eleft)

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawB: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    g = named_points.g = midPoint a, e
    h = named_points.h = midPoint b, f
    outline.drawLine g, h

    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, wide
    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, wide
    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    tls = outline.drawTouchingCircle ll_bl, ll_tl, a
    bls = outline.drawTouchingCircle c, ll_bl, ll_tl

    i = named_points.i = ll_tr.towards b, wide
    k = named_points.k = ll_br.towards d, wide
    outline.drawLine i, k

    l = named_points.l = intersect g, h, i, k

    tl_l = a.towards c, narrow
    tl_r = i.towards k, narrow
    outline.drawLine tl_l, tl_r

    ml_l = e.towards a, narrow
    ml_r = (intersect e, f, i, k).towards i, narrow

    m = named_points.m = ml_r

    outline.drawLine ml_l, ml_r
    outline.drawCircle l, ml_r
    tir = l.distance ml_r
    n = named_points.n = l.towards h, tir + wide
    tor = l.distance i
    toc = n.towards g, tor
    outline.drawCircle toc, n

    if variant == 'p'
      ll_brr = ll_br.towards d, serif
      brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr
    else if variant == 'r'
      ll_brr = ll_br.towards d, serif
      brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

      q = named_points.q = midPoint a, b
      r = named_points.r = midPoint c, d
      outline.drawLine q, r

      s = named_points.s = (vertCircle toc, tor, q.x).lower

      lr_c = d.towards c, serif
      outline.drawLine s, lr_c
      lr_lc = lr_c.towards c, wide
      lr_tl = intersect(e, f, s, lr_c).towards(e, wide)
      outline.drawLine lr_tl, lr_lc

      tu = outline.drawTouchingCircle s, lr_c, d
      tl = outline.drawTouchingCircle d, lr_lc, lr_tl
    else
      nt = new Point n.x, a.y
      nb = new Point n.x, c.y
      outline.drawLine nt, nb

      o = named_points.o = midPoint ml_l, c
      p = named_points.p = new Point d.x, o.y
      outline.drawLine o, p

      bl_l = c.towards a, narrow
      bl_r = nb.towards nt, narrow
      outline.drawLine bl_l, bl_r
      q = named_points.q = intersect i, k, bl_l, bl_r

      r = named_points.r = intersect o, p, nt, nb
      lir = o.distance e
      lic = r.towards o, lir
      outline.drawCircle lic, r

      s = named_points.s = r.towards p, wide
      lor = o.distance c
      loc = s.towards o, lor
      outline.drawCircle loc, s

      icr = wide * 2 / 3
      icc_l = bl_l.towards bl_r, serif + wide + icr
      icc = outline.drawTouchingCircle ll_tr, intersect(bl_l, bl_r, ll_tr, ll_br), icc_l

    render = new FillShape sys.ctx

    render.moveTo c
    render.addArc bls, true
    render.addArc tls, true

    if variant == 'p'
      render.addArc {from: new Point(toc.x, toc.y - tor), to: new Point(toc.x, toc.y + tor), center:toc}, false
      render.lineTo intersect(e, f, ll_tr, ll_br)
      render.addArc brs, true
      render.lineTo c

      render.moveTo intersect(tl_l, tl_r, ll_tr, ll_br)
      render.lineTo intersect(ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from: ml_r, to: tl_r, center:l}, true
    else if variant == 'r'
      render.addArc {from: new Point(toc.x, toc.y - tor), to:s, center:toc}, false
      render.addArc tu, true
      render.addArc tl, false
      render.lineTo lr_tl
      render.lineTo intersect(e, f, ll_tr, ll_br)
      render.addArc brs, true
      render.lineTo c

      render.moveTo intersect(tl_l, tl_r, ll_tr, ll_br)
      render.lineTo intersect(ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from: ml_r, to: tl_r, center:l}, true
    else
      pp = intersectCircles(toc, tor, loc, lor).left

      render.addArc {from:new Point(toc.x, toc.y - tor), to:pp, center:toc}, false
      render.addArc {from:pp, to:new Point(loc.x, loc.y + lor), center:loc}, false
      render.lineTo c

      render.moveTo intersect(tl_l, tl_r, ll_tr, ll_br)
      render.lineTo intersect(ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from:ml_r, to:tl_r, center:l}, true
      render.lineTo intersect(tl_l, tl_r, ll_tr, ll_br)

      render.moveTo intersect(e, f, ll_tr, ll_br)
      render.addArc icc, true
      render.addArc {from:new Point(lic.x, lic.y + lir), to:new Point(lic.x, lic.y - lir), center:lic}, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawC: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d

    outline.drawLine e, f

    g = named_points.g = b.towards a, wide
    h = named_points.h = d.towards c, wide
    outline.drawLine g, h

    outline.drawLine c, b
    i = named_points.i = midPoint e, f
    k = named_points.k = i.towards f, wide

    radius = i.distance e
    outline.drawCircle i, e
    outline.drawCircle k, f.towards(e, -wide)

    tr = f.towards e, wide / 2
    tl = d.towards c, wide / 2

    outline.drawLine tr, tl

    ik = midPoint i, k
    inner_radius = radius - narrow
    outline.drawCircle ik, ik.towards(e, inner_radius)

    render = new FillShape sys.ctx

    ubr = vertCircle(i, radius, tr.x).lower
    lbr = vertCircle(k, radius, tr.x).lower

    utr = vertCircle(k, radius, g.x).upper
    ltr = vertCircle(i, radius, g.x).upper

    render.moveTo ubr
    render.addArc {from:lbr, to:new Point(k.x, c.y), center:k}, false
    render.addArc {from: new Point(i.x, c.y), to: new Point(i.x, a.y), center:i}, false
    render.addArc {from: new Point(k.x, a.y), to: utr, center:k}, false
    render.lineTo ltr

    lo_in = intersectCircles i, radius, ik, inner_radius
    render.addArc {from: ltr, to: lo_in.left, center:i}, true

    ro_in = intersectCircles k, radius, ik, inner_radius
    render.addArc {from: lo_in.left, to: ro_in.right, center: ik}, true

    render.addArc {from: ro_in.right, to:ro_in.left, center: k}, true
    render.addArc {from:ro_in.left, to:lo_in.right, center:ik}, true
    render.addArc {from:lo_in.right, to: ubr, center:i}, true

    render.closeAndFill()

    outline.labelPoints named_points
    return

  drawD: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    g = named_points.g = midPoint a, b
    h = named_points.h = midPoint c, d
    outline.drawLine g, h

    i = named_points.i = intersect e, f, g, h

    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, wide

    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, wide

    outline.drawLine ll_bl, ll_tl
    outline.drawLine ll_br, ll_tr

    lls = outline.drawTouchingCircle c, ll_bl, ll_tl
    tls = outline.drawTouchingCircle ll_bl, ll_tl, a

    outline.drawCircle i, f
    k = named_points.k = f.towards e, wide

    tl_l = a.towards c, narrow
    tl_r = g.towards h, narrow
    outline.drawLine tl_l, tl_r

    bl_l = c.towards a, narrow
    bl_r = h.towards g, narrow
    outline.drawLine bl_l, bl_r

    innerRadius = tl_l.distance e
    l = named_points.l = k.towards e, innerRadius

    outline.drawCircle l, k

    isp = bl_l.towards bl_r, serif + wide + serif
    _is = outline.drawTouchingCircle ll_tr, intersect(ll_tr, ll_br, bl_l, bl_r), isp

    render = new FillShape sys.ctx

    render.moveTo c
    render.addArc lls, true
    render.addArc tls, true
    render.addArc {from:g, to:h, center:i}, false
    render.lineTo c

    render.moveTo intersect(ll_tr, ll_br, tl_l, tl_r)
    render.addArc _is, true
    render.addArc {to:new Point(l.x, tl_l.y), from:new Point(l.x, bl_l.y), center:l}, true

    render.closeAndFill()

    outline.labelPoints named_points
    return

  drawE: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    if variant != 'l'
      e = named_points.e = midPoint a, c
      f = named_points.f = midPoint b, d
      outline.drawLine e, f

    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, wide

    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, wide

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    tls = outline.drawTouchingCircle ll_bl, ll_tl, a
    bls = outline.drawTouchingCircle c, ll_bl, ll_tl
    if variant == 'l'
      trs = outline.drawTouchingCircle ll_tr.towards(b, serif), ll_tr, ll_br

    tl_tr = a.towards b, (6 - 1/3) * wide
    tl_trd = new Point tl_tr.x, c.y
    outline.drawLine tl_tr, tl_trd

    if variant != 'l'
      tl_l = a.towards c, narrow
      tl_r = tl_tr.towards tl_trd, narrow
      outline.drawLine tl_l, tl_r

      tlrs = outline.drawTouchingCircle tl_tr.towards(tl_trd, wide), tl_r, tl_l

      ml_l = e.towards a, narrow
      ml_lr = e.towards f, (5 - 1/3) * wide
      ml_r = new Point ml_lr.x, ml_l.y

      outline.drawLine ml_l, ml_r

      ml_rl = ml_r.towards ml_lr, wide + narrow / 2
      ml_ru = ml_rl.towards ml_r, 2 * wide

      outline.drawLine ml_ru, new Point(ml_r.x, c.y)
      mlus = outline.drawTouchingCircle ml_l, ml_r, ml_ru
      mlls = outline.drawTouchingCircle ml_rl, ml_lr, e

    if variant != 'f'
      bl_l = c.towards a, narrow
      bl_br = c.towards d, (7 - 1/3) * wide
      bl_r = new Point bl_br.x, bl_l.y

      outline.drawLine bl_l, bl_r
      blrs = outline.drawTouchingCircle intersect(tl_tr, tl_trd, bl_l, bl_r), bl_r, b
      outline.drawLine blrs.to, bl_br

      blls = outline.drawTouchingCircle ll_tr,
        intersect ll_tr, ll_br, bl_l, bl_r
        bl_l.towards bl_r, serif + wide + (wide - narrow / 2)
    else
      blls = outline.drawTouchingCircle ll_tr, ll_br, ll_br.towards(d, serif)

    render = new FillShape sys.ctx

    render.moveTo c
    render.addArc bls, true
    render.addArc tls, true
    if variant != 'l'
      render.lineTo tl_tr
      render.addArc tlrs, true
      render.lineTo intersect(tl_l, tl_r, ll_tr, ll_br)
      render.lineTo intersect(ll_tr, ll_br, ml_l, ml_r)
      render.addArc mlus, true
      render.addArc mlls, true
      render.lineTo intersect(ll_tr, ll_br, e, f)
    else 
      render.addArc trs, true

    render.addArc blls, true
    if variant != 'f'
      render.addArc blrs, true
      render.lineTo bl_br

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawG: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine(e, f)

    g = named_points.g = b.towards a, wide
    h = named_points.h = d.towards c, wide
    outline.drawLine g, h

    outline.drawLine b, c

    i = named_points.i = intersect b, c, e, f
    k = named_points.k = i.towards f, wide

    outline.drawCircle i, e
    outline.drawCircle k, e.towards(f, wide)

    rl_ll = h.towards c, wide
    rli = intersect e, f, g, h
    rl_tl = rli.towards e, wide
    outline.drawLine rl_ll, rl_tl

    rlls = outline.drawTouchingCircle rl_ll, rl_tl, rl_tl.towards(e, serif)
    rlrs = outline.drawTouchingCircle f, rli, h

    icc = midPoint i, k
    outerRadius = i.distance e
    icr = outerRadius - narrow
    icl = icc.towards e, icr
    outline.drawCircle icc, icl

    render = new FillShape sys.ctx

    gk_v = vertCircle k, outerRadius, g.x

    render.moveTo gk_v.upper

    lo_in = intersectCircles i, outerRadius, icc, icr
    ro_in = intersectCircles k, outerRadius, icc, icr
    lo_v = vertCircle i, outerRadius, i.x
    ro_v = vertCircle k, outerRadius, k.x

    render.addArc {from: vertCircle(icc, icr, g.x).upper, to:ro_in.right, center:icc}, true
    # addArc {from: lo_in.left, to: ro_in.right, center:icc}, true
    render.addArc {from: ro_in.right, to: ro_in.left, center:k}, true

    render.addArc {from:ro_in.left, to:vertCircle(icc, icr, rl_ll.x).lower, center:icc}, true
    render.addArc rlls, true
    render.addArc rlrs, true

    render.addArc {from:gk_v.lower, to:ro_v.lower, center:k}, false
    render.addArc {from:lo_v.lower, to:lo_v.upper, center:i}, false
    render.addArc {from:ro_v.upper, to:gk_v.upper, center:k}, false

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawH: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, wide
    ll_brs = ll_br.towards d, serif

    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, wide
    ll_trs = ll_tr.towards b, serif

    outline.drawLine ll_bl, ll_tl
    outline.drawLine ll_br, ll_tr
    lbls = outline.drawTouchingCircle c, ll_bl, ll_tl
    ltls = outline.drawTouchingCircle ll_bl, ll_tl, a
    ltrs = outline.drawTouchingCircle ll_trs, ll_tr, ll_br
    lbrs = outline.drawTouchingCircle ll_tr, ll_br, ll_brs

    rl_br = d.towards c, serif
    rl_bl = rl_br.towards c, wide
    rl_bls = rl_bl.towards c, serif

    rl_tr = b.towards a, serif
    rl_tl = rl_tr.towards a, wide
    rl_tls = rl_tl.towards a, serif

    outline.drawLine rl_tr, rl_br
    outline.drawLine rl_tl, rl_bl

    rbls = outline.drawTouchingCircle rl_bls, rl_bl, rl_tl
    rtls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tls
    rtrs = outline.drawTouchingCircle b, rl_tr, rl_br
    rbrs = outline.drawTouchingCircle rl_tr, rl_br, d

    ml_l = e.towards a, narrow
    ml_r = f.towards b, narrow
    outline.drawLine ml_l, ml_r

    render = new FillShape sys.ctx

    render.moveTo c
    render.addArc lbls, true
    render.addArc ltls, true
    render.addArc ltrs, true
    render.lineTo intersect(ml_l, ml_r, ll_tr, ll_br)
    render.lineTo intersect(ml_l, ml_r, rl_tl, rl_bl)
    render.addArc rtls, true
    render.addArc rtrs, true
    render.addArc rbrs, true
    render.addArc rbls, true
    render.lineTo intersect(e, f, rl_tl, rl_bl)
    render.lineTo intersect(e, f, ll_tr, ll_br)
    render.addArc lbrs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawI: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}

    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    ab = midPoint a, b
    cd = midPoint c, d

    bl = cd.towards c, wide / 2
    bll = bl.towards c, serif

    br = cd.towards d, wide / 2
    brr = br.towards d, serif

    tl = ab.towards a, wide / 2
    tll = tl.towards a, serif

    tr = ab.towards b, wide / 2
    trr = tr.towards b, serif

    outline.drawLine tl, bl
    outline.drawLine tr, br

    bls = outline.drawTouchingCircle bll, bl, tl
    tls = outline.drawTouchingCircle bl, tl, tll
    trs = outline.drawTouchingCircle trr, tr, br
    brs = outline.drawTouchingCircle tr, br, brr

    render = new FillShape sys.ctx

    render.moveTo bll
    render.addArc bls, true
    render.addArc tls, true
    render.addArc trs, true
    render.addArc brs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawK: (id, variant, proportions) ->
    Util.initializeCanvas(id)

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, wide
    ll_brr = ll_br.towards d, serif

    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, wide
    ll_trr = ll_tr.towards b, serif

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    lls = outline.drawTouchingCircle c, ll_bl, ll_tl
    uls = outline.drawTouchingCircle ll_bl, ll_tl, a
    urs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br
    lrs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    rul_l = intersect e, f, ll_tr, ll_br
    rul_r = intersect a, b, rul_l, (new Point rul_l.x + b.x - c.x, rul_l.y + b.y - c.y)
    outline.drawLine rul_l, rul_r
    rul_rr = rul_r.towards b, serif
    rul_rs = outline.drawTouchingCircle rul_rr, rul_r, rul_l

    temp = rul_l.towards (new Point rul_l.x + a.x - d.x, rul_l.y + a.y - d.y), narrow
    rul_ll = intersect ll_tr, ll_br, temp, (new Point temp.x + b.x - c.x, temp.y + b.y - c.y)
    rul_rl = intersect a,b,temp, rul_ll
    outline.drawLine rul_ll, rul_rl
    rul_rll = rul_rl.towards a, serif 
    rul_ls = outline.drawTouchingCircle rul_ll, rul_rl, rul_rll

    rll_lr = intersect c, d, rul_ll, (new Point rul_ll.x + d.x - a.x, rul_ll.y + d.y - a.y)
    rll_ll = intersect rul_l, rul_r, rul_ll, rll_lr
    outline.drawLine rll_lr, rll_ll

    rll_lt = outline.drawTouchingCircle d, rll_lr, rll_ll
    named_points.g = rll_lt.center

    rll_ul = rll_ll.towards rul_r, wide
    rll_ur = intersect c, d, rll_ul, (new Point rll_ul.x + d.x - a.x, rll_ul.y + d.y - a.y)
    outline.drawLine rll_ur, rll_ul

    rll_ut = outline.drawTouchingCircle rll_ul, rll_ur, d
    named_points.h = rll_ut.center

    render = new FillShape sys.ctx
    render.moveTo c
    render.addArc lls, true
    render.addArc uls, true
    render.addArc urs, true
    render.lineTo rul_ll
    render.addArc rul_ls, true
    render.addArc rul_rs, true
    render.lineTo rll_ul
    render.addArc rll_ut, true
    render.addArc rll_lt, false
    render.lineTo rll_ll
    render.lineTo rul_l
    render.addArc lrs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawM: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    dab = a.distance b
    f = named_points.f = a.towards b, dab / 6
    g = named_points.g = b.towards a, dab / 6

    ll_tl = f.towards a, narrow
    outline.drawLine ll_tl, c
    ll_br = c.towards d, narrow
    outline.drawLine f, ll_br
    ll_bll = c.towards d, -serif
    ll_brr = ll_br.towards d, serif
    ll_ls = outline.drawTouchingCircle ll_bll, c, ll_tl
    ll_rs = outline.drawTouchingCircle f, ll_br, ll_brr
    ll_ts = outline.drawTouchingCircle c, ll_tl, (a.towards b, -narrow)

    rl_tr = g.towards b, narrow
    outline.drawLine rl_tr, d
    rl_tl = rl_tr.towards a, wide
    rl_bl = d.towards c, wide
    outline.drawLine rl_tl, rl_bl
    rl_bll = rl_bl.towards c, serif
    rl_ls = outline.drawTouchingCircle rl_bll, rl_bl, rl_tl
    rl_brr = d.towards c, -serif
    rl_rs = outline.drawTouchingCircle rl_tr, d, rl_brr
    rl_ts = outline.drawTouchingCircle (b.towards a, narrow), rl_tr, d

    e = named_points.e = midPoint c, d # ll_br, rl_bl

    ml_lt = f.towards a, wide
    outline.drawLine ml_lt, e
    ml_rb = e.towards d, wide
    outline.drawLine f, ml_rb

    mr_lb = e.towards c, narrow
    outline.drawLine g, mr_lb
    mr_rt = g.towards b, narrow
    outline.drawLine mr_rt, e

    render = new FillShape sys.ctx
    render.moveTo c
    render.addArc ll_ls, true
    render.addArc ll_ts, true
    render.lineTo f
    render.lineTo (intersect f, ml_rb, mr_lb, g)
    render.lineTo g
    render.addArc rl_ts, true
    render.addArc rl_rs, true
    render.addArc rl_ls, true
    render.lineTo (intersect rl_bl, rl_tl, e, mr_rt)
    render.lineTo e
    render.lineTo (intersect e, ml_lt, f, ll_br)
    render.addArc ll_rs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawN: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    ll_bl = c.towards d, serif
    ll_br = ll_bl.towards d, narrow
    ll_brr = ll_br.towards d, serif
    ll_tl = a.towards b, serif
    ll_tr = ll_tl.towards b, narrow

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    ll_ls = outline.drawTouchingCircle c, ll_bl, ll_tl
    ll_ts = outline.drawTouchingCircle ll_bl, ll_tl, (a.towards b, -serif / (Math.sqrt 2))
    ll_rs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    e = named_points.e = d.towards c, serif
    outline.drawLine a, e
    outline.drawLine ll_tr, d
    rl_bl = e.towards c, narrow
    rl_tr = b.towards a, serif
    rl_tl = rl_tr.towards a, narrow
    rl_tll = rl_tl.towards a, serif

    outline.drawLine rl_tl, rl_bl
    outline.drawLine rl_tr, e

    rl_ls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tll
    rl_rs = outline.drawTouchingCircle b, rl_tr, e

    render = new FillShape sys.ctx
    render.moveTo c
    render.addArc ll_ls, true
    render.addArc ll_ts, true
    render.lineTo ll_tr
    render.lineTo (intersect ll_tr, d, rl_bl, rl_tl)
    render.addArc rl_ls, true
    render.addArc rl_rs, true
    render.lineTo e
    render.lineTo (intersect e, a, ll_tr, ll_br)
    render.addArc ll_rs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawO: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    outline.drawLine c, b

    e = named_points.e = midPoint c, b
    f = named_points.f = e.towards c, wide / 2
    g = named_points.g = e.towards b, wide / 2

    ol = new Point c.x, f.y
    ob = new Point f.x, c.y
    _or = new Point b.x, g.y
    ot = new Point g.x, b.y

    radius = f.distance ol

    outline.drawCircle f, ob
    outline.drawCircle g, ot

    fgr = new Point f.y, g.x
    outline.drawCircle fgr, ot

    fgl = new Point f.x, g.y
    outline.drawCircle fgl, ob

    il = new Point g.x - radius, g.y
    it = new Point f.x, f.y - radius
    ib = new Point g.x, g.y + radius
    ir = new Point f.x + radius, f.y

    outline.drawCircle fgl, il
    outline.drawCircle fgr, ir

    if variant == 'q'
      outline.drawLine a, d
      tailRadius = c.distance d
      outline.drawLine d, (d.towards c, -tailRadius)
      h = named_points.h = new Point (d.towards c, -3 * tailRadius / 4).x, d.y + 0.01

      utc = (intersectCircles d, tailRadius, h, tailRadius).left
      outline.drawLine h, utc
      outline.drawLine d, utc
      outline.drawCircle utc, d

      llor = fgl.distance _or
      utci = (intersectCircles utc, tailRadius, fgl, llor).left
      outline.drawCircle utci, (new Point utci.x, utci.y + wide)
      ltci = (intersectCircles utci, wide, fgl, llor).left
      ltc = (intersectCircles ltci, tailRadius, h, tailRadius).left
      outline.drawLine ltci, ltc
      outline.drawLine h, ltc
      outline.drawCircle ltc, h

    render = new FillShape sys.ctx

    render.moveTo ob
    render.addArc {from:ob, to:ol, center:f}, false
    render.addArc {from:ol, to:ot, center:fgr}, false
    render.addArc {from:ot, to:_or, center:g}, false
    if variant != 'q'
      render.addArc({from:_or, to:ob, center:fgl}, false)
    else
      render.addArc {from:_or, to:utci, center:fgl}, false
      render.addArc {from:utci, to:h, center:utc}, true
      render.addArc {from:h, to:ltci, center:ltc}, false
      render.addArc {from:ltci, to:ob, center:fgl}, false

    render.moveTo ib
    render.addArc {from:ib, to:ir, center: fgr}, true
    render.addArc {from:ir, to:it, center: f}, true
    render.addArc {from:it, to:il, center: fgl}, true
    render.addArc {from:il, to:ib, center: g}, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawS: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    g = named_points.g = midPoint a, b
    h = named_points.h = midPoint c, d
    outline.drawLine g, h

    m = named_points.m = intersect e, f, g, h

    i = named_points.i = g.towards h, narrow
    k = named_points.k = h.towards g, narrow

    l = named_points.l = m.towards h, wide / 3
    n = named_points.n = m.towards g, 2 * wide / 3

    toc = midPoint g, l
    outline.drawCircle toc, g
    tic = midPoint i, n
    outline.drawCircle tic, i

    boc = midPoint n, h
    outline.drawCircle boc, h

    bic = midPoint l, k
    outline.drawCircle bic, k

    rtl = new Point tic.x + (tic.distance i), tic.y
    outline.drawCircle rtl, (rtl.towards tic, 4 * wide / 3)
    rtu = (intersectCircles rtl, 4 * wide / 3, toc, (toc.distance g)).left
    outline.drawLine rtl, rtu

    bir = bic.distance l
    bor = boc.distance n
    ltx = boc.x - (bir + bor) / 2
    lt = vertCircle boc, bor, ltx

    outline.drawLine lt.lower, lt.upper

    ltu = new Point lt.lower.x, boc.y
    outline.drawLine ltu, boc

    hyp = ltu.distance bic
    angle = (Math.acos bir / hyp) - (Math.acos ((bir + bor) / 2) / hyp)
    lti = new Point bic.x - (Math.cos angle) * bir, bic.y + (Math.sin angle) * bir
    outline.drawLine lti, ltu

    render = new FillShape sys.ctx

    render.moveTo ltu
    render.addArc {from:lti, to:l, center: bic}, true
    render.addArc {from:l, to:rtu, center:toc}, false
    render.addArc {from:rtl, to:n, center:tic}, true
    render.addArc {from:n, to:lt.lower, center:boc}, false

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawT: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    l_bl = (midPoint c, d).towards c, wide / 2
    l_br = l_bl.towards d, wide
    if variant != 'j'
      l_bll = l_bl.towards c, serif
      l_brr = l_br.towards d, serif

    l_tl = (midPoint a, b).towards a, wide / 2
    l_tr = l_tl.towards b, wide

    e = named_points.e = a.towards b, wide
    f = named_points.f = b.towards a, wide

    outline.drawLine l_tl, l_bl
    outline.drawLine l_tr, l_br
    if variant != 'j'
      bls = outline.drawTouchingCircle l_bll, l_bl, l_tl
      brs = outline.drawTouchingCircle l_tr, l_br, l_brr
    else
      lob = midPoint c, l_br
      lor = lob.distance c
      loc = new Point c.x + lor, c.y - lor
      outline.drawCircle loc, lob

      lilb = new Point e.x - narrow / 2, c.y
      lib = midPoint lilb, l_bl
      lir = lib.distance l_bl
      lic = new Point lib.x, c.y - lir - narrow
      outline.drawCircle lic, (new Point lib.x, lib.y - narrow)
      outline.drawLine e, (new Point e.x, c.y)

      ltu = (vertCircle lic, lir, e.x).lower
      ltl = (vertCircle loc, lor, e.x).lower

    tl_l = a.towards c, narrow
    tl_r = b.towards d, narrow
    outline.drawLine tl_l, tl_r

    tl_ll = new Point e.x - 3 * narrow, e.y + 2 * narrow
    tl_lu = new Point e.x + narrow, e.y - narrow
    outline.drawLine tl_ll, tl_lu

    tl_rl = new Point f.x - 2 * narrow, e.y + 2 * narrow
    tl_ru = new Point f.x + 2 * narrow, e.y - narrow
    outline.drawLine tl_rl, tl_ru

    tlls = outline.drawTouchingCircle tl_r, (intersect tl_l, tl_r, tl_ll, tl_lu), tl_ll
    tlus = outline.drawTouchingCircle tl_lu, (intersect a, b, tl_ll, tl_lu), b

    trus = outline.drawTouchingCircle a, (intersect a, b, tl_rl, tl_ru), tl_ru
    trls = outline.drawTouchingCircle tl_rl, (intersect tl_l, tl_r, tl_rl, tl_ru), tl_l

    render = new FillShape sys.ctx

    if variant != 'j'
      render.moveTo bls.from
      render.addArc bls, true
    else
      render.moveTo ltl
      render.addArc {from:ltu, to:(new Point l_bl.x, lic.y), center:lic}, true

    render.lineTo (intersect l_bl, l_tl, tl_l, tl_r)
    render.addArc tlls, true
    render.addArc tlus, true
    render.addArc trus, true
    render.addArc trls, true
    render.lineTo (intersect l_br, l_tr, tl_l, tl_r)
    if variant != 'j'
      render.addArc brs, true
    else
      render.addArc {from:(new Point l_br.x, loc.y), to:ltl, center:loc}, false

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawU: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint c, d
    f = named_points.f = a.towards b, serif
    g = named_points.g = c.towards d, serif
    outline.drawLine f, g

    h = named_points.h = b.towards a, serif
    i = named_points.i = d.towards c, serif
    outline.drawLine h, i

    ll_tr = f.towards b, wide
    ll_trr = ll_tr.towards b, serif
    ll_br = g.towards d, wide
    outline.drawLine ll_tr, ll_br
    tlls = outline.drawTouchingCircle g, f, a
    tlrs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br

    rl_tl = h.towards a, wide
    rl_tll = rl_tl.towards a, serif
    rl_bl = i.towards c, wide
    outline.drawLine rl_tl, rl_bl
    rlls = outline.drawTouchingCircle rl_bl, rl_tl,rl_tll
    rlrs = outline.drawTouchingCircle b, h, i

    lor = e.distance g
    loc = new Point e.x, e.y - lor
    outline.drawCircle loc, e

    lir = e.distance ll_br
    lic = new Point e.x, e.y - lir - narrow
    outline.drawCircle lic, (new Point e.x, e.y - narrow)

    render = new FillShape sys.ctx

    render.moveTo tlls.from
    render.addArc tlls, true
    render.addArc tlrs, true
    render.addArc {from:(new Point ll_br.x, lic.y), to:(new Point rl_bl.x, lic.y), center:lic}, true
    render.addArc rlls, true
    render.addArc rlrs, true
    render.addArc {from:(new Point h.x, loc.y), to:(new Point f.x, loc.y), center:loc}, false

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawV: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint c, d
    f = named_points.f = a.towards b, serif
    g = named_points.g = b.towards a, serif

    ll_tr = f.towards b, wide
    ll_trr = ll_tr.towards b, serif
    ll_br = e.towards d, wide

    outline.drawLine f, e
    outline.drawLine ll_tr, ll_br
    llls = outline.drawTouchingCircle e, f, a
    llrs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br

    rl_tl = g.towards a, narrow
    rl_tll = rl_tl.towards a, serif
    rl_bl = e.towards c, narrow

    outline.drawLine rl_tl, rl_bl
    outline.drawLine g, e
    rlls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tll
    rlrs = outline.drawTouchingCircle b, g, e

    render = new FillShape sys.ctx

    render.moveTo e
    render.addArc llls, true
    render.addArc llrs, true
    render.lineTo (intersect ll_tr, ll_br, rl_tl, rl_bl)
    render.addArc rlls, true
    render.addArc rlrs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawW: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, b
    fg = midPoint c, d
    f = named_points.f = midPoint c, fg
    g = named_points.g = midPoint fg, d

    f_l = f.towards c, narrow
    f_r = f.towards d, wide

    g_l = g.towards c, narrow
    g_r = g.towards d, wide

    a_l = a.towards b, -serif
    a_r = a.towards b, wide
    a_rr = a_r.towards b, serif

    outline.drawLine a, f
    outline.drawLine a_r, f_r

    e_l = e.towards a, wide
    e_r = e.towards b, narrow

    outline.drawLine e, f_l
    outline.drawLine e_r, f
    als = outline.drawTouchingCircle f, a, a_l
    ars = outline.drawTouchingCircle a_rr, a_r, f_r

    outline.drawLine e_l, g
    outline.drawLine e, g_r

    b_l = b.towards a, narrow
    b_ll = b_l.towards a, serif
    b_r = b.towards a, -serif

    outline.drawLine b_l, g_l
    outline.drawLine b, g
    bls = outline.drawTouchingCircle g_l, b_l, b_ll
    brs = outline.drawTouchingCircle b_r, b, g

    render = new FillShape sys.ctx

    render.moveTo f
    render.addArc als, true
    render.addArc ars, true
    render.lineTo (intersect a_r, f_r, f_l, e)
    render.lineTo e
    render.lineTo (intersect e, g_r, g_l, b_l)
    render.addArc bls, true
    render.addArc brs, true
    render.lineTo g
    render.lineTo (intersect g, e_l, e_r, f)

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawX: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    f = named_points.f = a.towards b, serif
    g = named_points.g = c.towards d, serif
    outline.drawLine f, g

    h = named_points.h = b.towards a, serif
    i = named_points.i = d.towards c, serif
    outline.drawLine h, i

    tl_r = f.towards b, wide
    tl_rr = tl_r.towards b, serif

    bl_r = g.towards d, narrow
    bl_rr = bl_r.towards d, serif

    tr_l = h.towards a, narrow
    tr_ll = tr_l.towards a, serif

    br_l = i.towards c, wide
    br_ll = br_l.towards c, serif

    outline.drawLine f, br_l
    outline.drawLine tl_r, i

    outline.drawLine tr_l, g
    outline.drawLine h, bl_r

    tlls = outline.drawTouchingCircle br_l, f, a
    tlrs = outline.drawTouchingCircle tl_rr, tl_r, i

    trls = outline.drawTouchingCircle g, tr_l, tr_ll
    trrs = outline.drawTouchingCircle b, h, bl_r

    blls = outline.drawTouchingCircle c, g, tr_l
    blrs = outline.drawTouchingCircle h, bl_r, bl_rr

    brrs = outline.drawTouchingCircle tl_r, i, d
    brls = outline.drawTouchingCircle br_ll, br_l, f

    render = new FillShape sys.ctx

    render.moveTo g
    render.addArc blls, true
    render.lineTo (intersect g, tr_l, f, br_l)
    render.addArc tlls, true
    render.addArc tlrs, true
    render.lineTo (intersect tl_r, i, g, tr_l)
    render.addArc trls, true
    render.addArc trrs, true
    render.lineTo (intersect h, bl_r, i, tl_r)
    render.addArc brrs, true
    render.addArc brls, true
    render.lineTo (intersect br_l, f, h, bl_r)
    render.addArc blrs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawY: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = midPoint a, c
    f = named_points.f = midPoint b, d
    outline.drawLine e, f

    ll_bl = (midPoint c, d).towards c, wide / 2
    ll_bll = ll_bl.towards c, serif
    ll_br = ll_bl.towards d, wide
    ll_brr = ll_br.towards d, serif

    ll_tl = (midPoint e, f).towards e, wide / 2
    ll_tr = ll_tl.towards f, wide

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    bls = outline.drawTouchingCircle ll_bll, ll_bl, ll_tl
    brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    tlm = ll_tl.towards ll_tr, 2 * wide / 3

    tl_l = a.towards b, serif
    tl_r = tl_l.towards b, 2 * wide / 3
    tl_rr = tl_r.towards b, serif

    tr_r = b.towards a, serif
    tr_l = tr_r.towards a, wide / 3
    tr_ll = tr_l.towards a, serif

    outline.drawLine tl_l, ll_tl
    outline.drawLine tl_r, tlm

    outline.drawLine tr_l, tlm
    outline.drawLine tr_r, ll_tr

    tlls = outline.drawTouchingCircle ll_tl, tl_l, a
    tlrs = outline.drawTouchingCircle tl_rr, tl_r, tlm

    trls = outline.drawTouchingCircle tlm, tr_l, tr_ll
    trrs = outline.drawTouchingCircle b, tr_r, ll_tr

    render = new FillShape sys.ctx

    render.moveTo (midPoint c, d)
    render.addArc bls, true
    render.lineTo ll_tl
    render.addArc tlls, true
    render.addArc tlrs, true
    render.lineTo tlm
    render.addArc trls, true
    render.addArc trrs, true
    render.lineTo ll_tr
    render.addArc brs, true

    render.closeAndFill()
    outline.labelPoints named_points
    return

  drawZ: (id, variant, proportions) ->
    Util.initializeCanvas id

    named_points = {}
    prop = Util.initializeSquare named_points, proportions.aspect
    wide = proportions.broad_stem * prop
    serif = proportions.serif * prop
    narrow = proportions.narrow_stem * wide

    a = named_points.a
    b = named_points.b
    c = named_points.c
    d = named_points.d

    outline = new OutlineDrawer sys.ctx

    outline.drawLine a, b
    outline.drawLine c, d
    outline.drawLine a, c
    outline.drawLine b, d

    e = named_points.e = a.towards c, serif
    f = named_points.f = a.towards b, serif
    outline.drawLine e, f

    g = named_points.g = d.towards c, serif
    h = named_points.h = d.towards b, serif
    outline.drawLine g, h

    tl_l = a.towards c, narrow
    tl_r = b.towards d, narrow
    outline.drawLine tl_l, tl_r
    tls = outline.drawTouchingCircle tl_r, (intersect tl_l, tl_r, e, f), e

    bl_l = c.towards a, narrow
    bl_r = d.towards b, narrow
    outline.drawLine bl_l, bl_r
    brs = outline.drawTouchingCircle bl_l, (intersect bl_l, bl_r, g, h), h

    ml_br = c.towards d, wide * 1.25
    ml_tl = b.towards a, wide * 1.25

    outline.drawLine ml_tl, c
    outline.drawLine b, ml_br

    render = new FillShape sys.ctx

    render.moveTo c
    render.lineTo (intersect c, ml_tl, tl_l, tl_r)
    render.addArc tls, true
    render.lineTo f
    render.lineTo b
    render.lineTo (intersect b, ml_br, bl_l, bl_r)
    render.addArc brs, true
    render.lineTo g

    render.closeAndFill()
    outline.labelPoints named_points
    return

  makeFuncs: () ->
    @funcs = 
      a:@drawA
      b:@drawB
      c:@drawC
      d:@drawD
      e:@drawE
      f:@drawE
      g:@drawG
      h:@drawH
      i:@drawI
      j:@drawT
      k:@drawK
      l:@drawE
      m:@drawM
      n:@drawN
      o:@drawO
      p:@drawB
      q:@drawO
      r:@drawB
      s:@drawS
      t:@drawT
      u:@drawU
      v:@drawV
      w:@drawW
      x:@drawX
      y:@drawY
      z:@drawZ

  drawLetter: (id, letter, proportions) ->
    letter = letter.toLowerCase()

    f = @funcs[letter]
    if f != undefined
      try f id, letter, proportions
      catch e then console.log 'Exception', e, 'drawing', letter
    else
      console.log 'Cannot find rendering functions for', letter
    return

proportions = {
  aspect: 1
  broad_stem: 0.111
  narrow_stem: 0.33
  serif: 0.111

  readFromDocument: () ->
    broad = 
    @broad_stem = ((document.getElementById 'broadProp').value | 0) / 1000.0
    @narrow_stem = ((document.getElementById 'narrowProp').value | 0) / 100.0
    @serif = ((document.getElementById 'serifProp').value | 0) / 1000.0
    return
}

getLetterShape = (letter, proportions) ->
    letter = letter.toLowerCase()
    f = GlyphFunctions.funcs[letter]

    if f != undefined
      oldOutline = sys.showOutlines
      sys.showOutlines = false
      f undefined, letter, proportions
      sys.showOutlines = oldOutline
      [sys.ctx.path, sys.ctx.lx, sys.ctx.rx, sys.ctx.leftLimits, sys.ctx.rightLimits]
    else
      ''

drawProofs = () ->
  GlyphFunctions.drawLetter 'glyph-A', 'A', proportions
  GlyphFunctions.drawLetter 'glyph-B', 'B', proportions
  GlyphFunctions.drawLetter 'glyph-C', 'C', proportions
  GlyphFunctions.drawLetter 'glyph-D', 'D', proportions
  GlyphFunctions.drawLetter 'glyph-E', 'E', proportions
  GlyphFunctions.drawLetter 'glyph-F', 'F', proportions
  GlyphFunctions.drawLetter 'glyph-G', 'G', proportions
  GlyphFunctions.drawLetter 'glyph-H', 'H', proportions
  GlyphFunctions.drawLetter 'glyph-I', 'I', proportions
  GlyphFunctions.drawLetter 'glyph-J', 'J', proportions
  GlyphFunctions.drawLetter 'glyph-K', 'K', proportions
  GlyphFunctions.drawLetter 'glyph-L', 'L', proportions
  GlyphFunctions.drawLetter 'glyph-M', 'M', proportions
  GlyphFunctions.drawLetter 'glyph-N', 'N', proportions
  GlyphFunctions.drawLetter 'glyph-O', 'O', proportions
  GlyphFunctions.drawLetter 'glyph-P', 'P', proportions
  GlyphFunctions.drawLetter 'glyph-Q', 'Q', proportions
  GlyphFunctions.drawLetter 'glyph-R', 'R', proportions
  GlyphFunctions.drawLetter 'glyph-S', 'S', proportions
  GlyphFunctions.drawLetter 'glyph-T', 'T', proportions
  GlyphFunctions.drawLetter 'glyph-U', 'U', proportions
  GlyphFunctions.drawLetter 'glyph-V', 'V', proportions
  GlyphFunctions.drawLetter 'glyph-W', 'W', proportions
  GlyphFunctions.drawLetter 'glyph-X', 'X', proportions
  GlyphFunctions.drawLetter 'glyph-Y', 'Y', proportions
  GlyphFunctions.drawLetter 'glyph-Z', 'Z', proportions
  return

drawSampleText = () ->
    text = (document.getElementById 'sampleText').value
    offset = 0
    kernOffset = [0,0,0,0,0]

    for i in [0...text.length]
      letter = text[i]
      id = 'demo' + i
      path = document.getElementById id
      if path == undefined
        continue

      shape = getLetterShape letter, proportions
      if shape == ''
        console.log 'Cannot get letter shape for:', letter
        continue

      left = shape[1]
      width = shape[2] - shape[1]
      if width > 1
        width = 1

      leftLim = shape[3]
      rightLim = shape[4]

      kleft = 100000
      for k in [0...5]
        kleft = Math.min kleft, leftLim[k] - kernOffset[k]
        kernOffset[k] = rightLim[k] - 1

      offset -= kleft

      path.setAttribute 'd', shape[0]
      path.setAttribute 'transform', 'translate(' + offset + ')'

      offset += 1.1

    if text.length < 10
      for i in [text.length...10]
        path = document.getElementById 'demo' + i
        path.setAttribute 'd', 'M 0 0'

    return

onEvent = (id, event, callback) ->
  element = document.getElementById id
  if element.addEventListener?
    element.addEventListener event, callback, false
  else
    element.attachEvent 'on' + event, callback
  return

bindValueToText = (from, to) ->
  source = document.getElementById from
  target = document.getElementById to
  onEvent from, 'change', () -> target.innerHTML = source.value
  onEvent from, 'input', () -> target.innerHTML = source.value

onload = () ->
  GlyphFunctions.makeFuncs()

  changeProportions()
  drawProofs()
  drawSampleText()

  bindValueToText 'broadProp', 'broadPropValue'
  bindValueToText 'serifProp', 'serifPropValue'
  bindValueToText 'narrowProp', 'narrowPropValue'

  onEvent 'broadProp', 'change', changeProportions
  onEvent 'serifProp', 'change', changeProportions
  onEvent 'narrowProp', 'change', changeProportions
  onEvent 'broadProp', 'input', changeProportions
  onEvent 'serifProp', 'input', changeProportions
  onEvent 'narrowProp', 'input', changeProportions
  onEvent 'showProofs', 'change', showProofsChanged
  onEvent 'fillGlyph', 'change', redrawProofs
  onEvent 'strokeGlyph', 'change', redrawProofs
  onEvent 'overshoot', 'change', redrawProofs
  onEvent 'showOutlines', 'change', redrawProofs
  onEvent 'glyphColor', 'change', redrawProofs
  onEvent 'proofSize', 'change', changeProofSize
  onEvent 'proofSize', 'input', changeProofSize
  onEvent 'showSample', 'change', showSampleChanged
  onEvent 'sampleText', 'change', changeSampleText
  return

onload()
