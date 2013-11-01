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

  width: 0
  height: 0
  x_margin: 0
  y_margin: 0

  working_width: 0
  working_height: 0

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
      center: cc
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
  initializeGlyph: (id, proportions) ->
    Util.initializeCanvas id

    @labels = {}
    prop = Util.initializeSquare @labels, proportions.aspect
    @wide = proportions.broad_stem * prop
    @serif = proportions.serif * prop
    @narrow = proportions.narrow_stem * @wide
    return

  drawA: (id, variant, proportions) ->
    if variant != 'b' and variant != 'c'
        variant = 'a'

    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.b
    @labels.f = midPoint @labels.c, @labels.d
    outline.drawLine @labels.e, @labels.f

    @labels.g = midPoint @labels.a, @labels.c
    @labels.h = midPoint @labels.b, @labels.d
    outline.drawLine @labels.g, @labels.h

    @labels.i = @labels.c.towards @labels.d, @serif
    @labels.k = @labels.d.towards @labels.c, @serif

    eleft = @labels.e.towards @labels.a, @narrow
    eright = eleft.towards @labels.b, @wide

    if variant == 'b'
        eleft = @labels.e.towards @labels.a, @wide
        eright = @labels.e.towards @labels.b, @narrow

    if variant == 'a'
      outline.drawLine @labels.i, eleft
      outline.drawLine @labels.k, eright
    else if variant == 'b'
      outline.drawLine @labels.i, @labels.e
      outline.drawLine @labels.e, @labels.k
    else if variant == 'c'
      outline.drawLine @labels.i, eleft
      outline.drawLine @labels.k, eright

    iright = @labels.i.towards @labels.d, @narrow
    kleft = @labels.k.towards @labels.c, @wide

    if variant == 'a'
      outline.drawLine iright, @labels.e
      outline.drawLine kleft, eleft
    else if variant == 'b'
      outline.drawLine iright, eright
      outline.drawLine eleft, kleft
    else if variant == 'c'
      outline.drawLine iright, @labels.e
      outline.drawLine kleft, eleft

    gdown = @labels.g.towards @labels.c, @narrow
    hdown = @labels.h.towards @labels.d, @narrow
    outline.drawLine gdown, hdown

    isl = outline.drawTouchingCircle @labels.c, @labels.i, eleft
    isr = outline.drawTouchingCircle @labels.e, iright, (iright.towards @labels.d, @serif)

    ksr = outline.drawTouchingCircle eright, @labels.k, @labels.d
    ksl = outline.drawTouchingCircle (kleft.towards @labels.c, @serif), kleft, eleft

    if variant == 'a'
      ert = eright.towards @labels.k, @serif / 2
      outline.drawLine eright, ert
      dx = ert.x - eright.x
      dy = ert.y - eright.y
      tcc = new Point eright.x + dy, eright.y - dx
      outline.drawLine eright, tcc
      outline.drawCircle tcc, eright
      tcct = new Point tcc.x - dx, tcc.y - dy
      outline.drawLine tcc, tcct

      outline.drawBezier tcct, (tcct.towards @labels.a, @serif / 2), (eleft.towards @labels.k, @serif / 2), eleft
    else if variant == 'c'
        esl = outline.drawTouchingCircle @labels.i, eleft, (eleft.towards @labels.a, @serif)

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.addArc isl, true
    if variant == 'a'
      render.lineTo eleft
      render.addBezier (eleft.towards @labels.k, @serif / 2), (tcct.towards @labels.a, @serif / 2), tcct
      render.addArc {from: tcct, to: eright, center: tcc}, true
    else if variant == 'b'
        render.lineTo @labels.e
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
      render.lineTo intersect(iright, @labels.e, gdown, hdown)
    else if variant == 'b'
      render.lineTo intersect(iright, eright, gdown, hdown)

    render.lineTo isr.from
    render.addArc isr, true
    render.lineTo @labels.c

    if variant == 'a' or variant == 'c'
      render.moveTo (intersect @labels.g, @labels.h, iright, @labels.e)
      render.lineTo (intersect kleft, eleft, @labels.g, @labels.h)
      render.lineTo (intersect iright, @labels.e, kleft, eleft)
    else if variant == 'b'
      render.moveTo (intersect @labels.g, @labels.h, iright, eright)
      render.lineTo (intersect kleft, eleft, @labels.g, @labels.h)
      render.lineTo (intersect iright, eright, kleft, eleft)

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawB: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    @labels.g = midPoint @labels.a, @labels.e
    @labels.h = midPoint @labels.b, @labels.f
    outline.drawLine @labels.g, @labels.h

    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @wide
    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @wide
    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    tls = outline.drawTouchingCircle ll_bl, ll_tl, @labels.a
    bls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl

    @labels.i = ll_tr.towards @labels.b, @wide
    @labels.k = ll_br.towards @labels.d, @wide
    outline.drawLine @labels.i, @labels.k

    @labels.l = intersect @labels.g, @labels.h, @labels.i, @labels.k

    tl_l = @labels.a.towards @labels.c, @narrow
    tl_r = @labels.i.towards @labels.k, @narrow
    outline.drawLine tl_l, tl_r

    ml_l = @labels.e.towards @labels.a, @narrow
    ml_r = (intersect @labels.e, @labels.f, @labels.i, @labels.k).towards @labels.i, @narrow

    @labels.m = ml_r

    outline.drawLine ml_l, ml_r
    outline.drawCircle @labels.l, ml_r
    tir = @labels.l.distance ml_r
    @labels.n = @labels.l.towards @labels.h, tir + @wide
    tor = @labels.l.distance @labels.i
    toc = @labels.n.towards @labels.g, tor
    outline.drawCircle toc, @labels.n

    if variant == 'p'
      ll_brr = ll_br.towards @labels.d, @serif
      brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr
    else if variant == 'r'
      ll_brr = ll_br.towards @labels.d, @serif
      brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

      @labels.q = midPoint @labels.a, @labels.b
      @labels.r = midPoint @labels.c, @labels.d
      outline.drawLine @labels.q, @labels.r

      @labels.s = (vertCircle toc, tor, @labels.q.x).lower

      lr_c = @labels.d.towards @labels.c, @serif
      outline.drawLine @labels.s, lr_c
      lr_lc = lr_c.towards @labels.c, @wide
      lr_tl = (intersect @labels.e, @labels.f, @labels.s, lr_c).towards @labels.e, @wide
      outline.drawLine lr_tl, lr_lc

      tu = outline.drawTouchingCircle @labels.s, lr_c, @labels.d
      tl = outline.drawTouchingCircle @labels.d, lr_lc, lr_tl
    else
      nt = new Point @labels.n.x, @labels.a.y
      nb = new Point @labels.n.x, @labels.c.y
      outline.drawLine nt, nb

      @labels.o = midPoint ml_l, @labels.c
      @labels.p = new Point @labels.d.x, @labels.o.y
      outline.drawLine @labels.o, @labels.p

      bl_l = @labels.c.towards @labels.a, @narrow
      bl_r = nb.towards nt, @narrow
      outline.drawLine bl_l, bl_r
      @labels.q = intersect @labels.i, @labels.k, bl_l, bl_r

      @labels.r = intersect @labels.o, @labels.p, nt, nb
      lir = @labels.o.distance @labels.e
      lic = @labels.r.towards @labels.o, lir
      outline.drawCircle lic, @labels.r

      @labels.s = @labels.r.towards @labels.p, @wide
      lor = @labels.o.distance @labels.c
      loc = @labels.s.towards @labels.o, lor
      outline.drawCircle loc, @labels.s

      icr = @wide * 2 / 3
      icc_l = bl_l.towards bl_r, @serif + @wide + icr
      icc = outline.drawTouchingCircle ll_tr, intersect(bl_l, bl_r, ll_tr, ll_br), icc_l

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.addArc bls, true
    render.addArc tls, true

    if variant == 'p'
      render.addArc {from: new Point(toc.x, toc.y - tor), to: new Point(toc.x, toc.y + tor), center: toc}, false
      render.lineTo (intersect @labels.e, @labels.f, ll_tr, ll_br)
      render.addArc brs, true
      render.lineTo @labels.c

      render.moveTo (intersect tl_l, tl_r, ll_tr, ll_br)
      render.lineTo (intersect ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from: ml_r, to: tl_r, center:@labels.l}, true
    else if variant == 'r'
      render.addArc {from: new Point(toc.x, toc.y - tor), to:@labels.s, center: toc}, false
      render.addArc tu, true
      render.addArc tl, false
      render.lineTo lr_tl
      render.lineTo (intersect @labels.e, @labels.f, ll_tr, ll_br)
      render.addArc brs, true
      render.lineTo @labels.c

      render.moveTo (intersect tl_l, tl_r, ll_tr, ll_br)
      render.lineTo (intersect ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from: ml_r, to: tl_r, center:@labels.l}, true
    else
      pp = (intersectCircles toc, tor, loc, lor).left

      render.addArc {from: new Point(toc.x, toc.y - tor), to: pp, center: toc}, false
      render.addArc {from: pp, to: new Point(loc.x, loc.y + lor), center: loc}, false
      render.lineTo @labels.c

      render.moveTo (intersect tl_l, tl_r, ll_tr, ll_br)
      render.lineTo (intersect ml_l, ml_r, ll_tr, ll_br)
      render.addArc {from: ml_r, to: tl_r, center:@labels.l}, true
      render.lineTo (intersect tl_l, tl_r, ll_tr, ll_br)

      render.moveTo (intersect @labels.e, @labels.f, ll_tr, ll_br)
      render.addArc icc, true
      render.addArc {from: new Point(lic.x, lic.y + lir), to: new Point(lic.x, lic.y - lir), center: lic}, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawC: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d

    outline.drawLine @labels.e, @labels.f

    @labels.g = @labels.b.towards @labels.a, @wide
    @labels.h = @labels.d.towards @labels.c, @wide
    outline.drawLine @labels.g, @labels.h

    outline.drawLine @labels.c, @labels.b
    @labels.i = midPoint @labels.e, @labels.f
    @labels.k = @labels.i.towards @labels.f, @wide

    radius = @labels.i.distance @labels.e
    outline.drawCircle @labels.i, @labels.e
    outline.drawCircle @labels.k, (@labels.f.towards @labels.e, -@wide)

    tr = @labels.f.towards @labels.e, @wide / 2
    tl = @labels.d.towards @labels.c, @wide / 2

    outline.drawLine tr, tl

    ik = midPoint @labels.i, @labels.k
    inner_radius = radius - @narrow
    outline.drawCircle ik, (ik.towards @labels.e, inner_radius)

    render = new FillShape sys.ctx

    ubr = (vertCircle @labels.i, radius, tr.x).lower
    lbr = (vertCircle @labels.k, radius, tr.x).lower

    utr = (vertCircle @labels.k, radius, @labels.g.x).upper
    ltr = (vertCircle @labels.i, radius, @labels.g.x).upper

    render.moveTo ubr
    render.addArc {from: lbr, to: (new Point @labels.k.x, @labels.c.y), center: @labels.k}, false
    render.addArc {from: (new Point @labels.i.x, @labels.c.y), to: (new Point @labels.i.x, @labels.a.y), center: @labels.i}, false
    render.addArc {from: (new Point @labels.k.x, @labels.a.y), to: utr, center: @labels.k}, false
    render.lineTo ltr

    lo_in = intersectCircles @labels.i, radius, ik, inner_radius
    render.addArc {from: ltr, to: lo_in.left, center: @labels.i}, true

    ro_in = intersectCircles @labels.k, radius, ik, inner_radius
    render.addArc {from: lo_in.left, to: ro_in.right, center: ik}, true

    render.addArc {from: ro_in.right, to: ro_in.left, center: @labels.k}, true
    render.addArc {from: ro_in.left, to: lo_in.right, center: ik}, true
    render.addArc {from: lo_in.right, to: ubr, center: @labels.i}, true

    render.closeAndFill()

    outline.labelPoints @labels
    return

  drawD: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = @labels.e = midPoint @labels.a, @labels.c
    @labels.f = @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    @labels.g = @labels.g = midPoint @labels.a, @labels.b
    @labels.h = @labels.h = midPoint @labels.c, @labels.d
    outline.drawLine @labels.g, @labels.h

    @labels.i = @labels.i = intersect @labels.e, @labels.f, @labels.g, @labels.h

    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @wide

    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @wide

    outline.drawLine ll_bl, ll_tl
    outline.drawLine ll_br, ll_tr

    lls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl
    tls = outline.drawTouchingCircle ll_bl, ll_tl, @labels.a

    outline.drawCircle @labels.i, @labels.f
    @labels.k = @labels.k = @labels.f.towards @labels.e, @wide

    tl_l = @labels.a.towards @labels.c, @narrow
    tl_r = @labels.g.towards @labels.h, @narrow
    outline.drawLine tl_l, tl_r

    bl_l = @labels.c.towards @labels.a, @narrow
    bl_r = @labels.h.towards @labels.g, @narrow
    outline.drawLine bl_l, bl_r

    innerRadius = tl_l.distance @labels.e
    @labels.l = @labels.l = @labels.k.towards @labels.e, innerRadius

    outline.drawCircle @labels.l, @labels.k

    isp = bl_l.towards bl_r, @serif + @wide + @serif
    _is = outline.drawTouchingCircle ll_tr, (intersect ll_tr, ll_br, bl_l, bl_r), isp

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.addArc lls, true
    render.addArc tls, true
    render.addArc {from: @labels.g, to: @labels.h, center: @labels.i}, false
    render.lineTo @labels.c

    render.moveTo intersect(ll_tr, ll_br, tl_l, tl_r)
    render.addArc _is, true
    render.addArc {to: (new Point @labels.l.x, tl_l.y), from: (new Point @labels.l.x, bl_l.y), center: @labels.l}, true

    render.closeAndFill()

    outline.labelPoints @labels
    return

  drawE: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    if variant != 'l'
      @labels.e = @labels.e = midPoint @labels.a, @labels.c
      @labels.f = @labels.f = midPoint @labels.b, @labels.d
      outline.drawLine @labels.e, @labels.f

    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @wide

    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @wide

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    tls = outline.drawTouchingCircle ll_bl, ll_tl, @labels.a
    bls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl
    if variant == 'l'
      trs = outline.drawTouchingCircle (ll_tr.towards @labels.b, @serif), ll_tr, ll_br

    tl_tr = @labels.a.towards @labels.b, (6 - 1/3) * @wide
    tl_trd = new Point tl_tr.x, @labels.c.y
    outline.drawLine tl_tr, tl_trd

    if variant != 'l'
      tl_l = @labels.a.towards @labels.c, @narrow
      tl_r = tl_tr.towards tl_trd, @narrow
      outline.drawLine tl_l, tl_r

      tlrs = outline.drawTouchingCircle (tl_tr.towards tl_trd, @wide), tl_r, tl_l

      ml_l = @labels.e.towards @labels.a, @narrow
      ml_lr = @labels.e.towards @labels.f, (5 - 1/3) * @wide
      ml_r = new Point ml_lr.x, ml_l.y

      outline.drawLine ml_l, ml_r

      ml_rl = ml_r.towards ml_lr, @wide + @narrow / 2
      ml_ru = ml_rl.towards ml_r, 2 * @wide

      outline.drawLine ml_ru, (new Point ml_r.x, @labels.c.y)
      mlus = outline.drawTouchingCircle ml_l, ml_r, ml_ru
      mlls = outline.drawTouchingCircle ml_rl, ml_lr, @labels.e

    if variant != 'f'
      bl_l = @labels.c.towards @labels.a, @narrow
      bl_br = @labels.c.towards @labels.d, (7 - 1/3) * @wide
      bl_r = new Point bl_br.x, bl_l.y

      outline.drawLine bl_l, bl_r
      blrs = outline.drawTouchingCircle (intersect tl_tr, tl_trd, bl_l, bl_r), bl_r, @labels.b
      outline.drawLine blrs.to, bl_br

      blls = outline.drawTouchingCircle ll_tr,
        intersect ll_tr, ll_br, bl_l, bl_r
        bl_l.towards bl_r, @serif + @wide + (@wide - @narrow / 2)
    else
      blls = outline.drawTouchingCircle ll_tr, ll_br, (ll_br.towards @labels.d, @serif)

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.addArc bls, true
    render.addArc tls, true
    if variant != 'l'
      render.lineTo tl_tr
      render.addArc tlrs, true
      render.lineTo (intersect tl_l, tl_r, ll_tr, ll_br)
      render.lineTo (intersect ll_tr, ll_br, ml_l, ml_r)
      render.addArc mlus, true
      render.addArc mlls, true
      render.lineTo (intersect ll_tr, ll_br, @labels.e, @labels.f)
    else 
      render.addArc trs, true

    render.addArc blls, true
    if variant != 'f'
      render.addArc blrs, true
      render.lineTo bl_br

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawG: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    @labels.g = @labels.b.towards @labels.a, @wide
    @labels.h = @labels.d.towards @labels.c, @wide
    outline.drawLine @labels.g, @labels.h

    outline.drawLine @labels.b, @labels.c

    @labels.i = intersect @labels.b, @labels.c, @labels.e, @labels.f
    @labels.k = @labels.i.towards @labels.f, @wide

    outline.drawCircle @labels.i, @labels.e
    outline.drawCircle @labels.k, (@labels.e.towards @labels.f, @wide)

    rl_ll = @labels.h.towards @labels.c, @wide
    rli = intersect @labels.e, @labels.f, @labels.g, @labels.h
    rl_tl = rli.towards @labels.e, @wide
    outline.drawLine rl_ll, rl_tl

    rlls = outline.drawTouchingCircle rl_ll, rl_tl, (rl_tl.towards @labels.e, @serif)
    rlrs = outline.drawTouchingCircle @labels.f, rli, @labels.h

    icc = midPoint @labels.i, @labels.k
    outerRadius = @labels.i.distance @labels.e
    icr = outerRadius - @narrow
    icl = icc.towards @labels.e, icr
    outline.drawCircle icc, icl

    render = new FillShape sys.ctx

    gk_v = vertCircle @labels.k, outerRadius, @labels.g.x

    render.moveTo gk_v.upper

    lo_in = intersectCircles @labels.i, outerRadius, icc, icr
    ro_in = intersectCircles @labels.k, outerRadius, icc, icr
    lo_v = vertCircle @labels.i, outerRadius, @labels.i.x
    ro_v = vertCircle @labels.k, outerRadius, @labels.k.x

    render.addArc {from: (vertCircle icc, icr, @labels.g.x).upper, to: ro_in.right, center: icc}, true
    # addArc {from: lo_in.left, to: ro_in.right, center: icc}, true
    render.addArc {from: ro_in.right, to: ro_in.left, center: @labels.k}, true

    render.addArc {from: ro_in.left, to: (vertCircle icc, icr, rl_ll.x).lower, center: icc}, true
    render.addArc rlls, true
    render.addArc rlrs, true

    render.addArc {from: gk_v.lower, to: ro_v.lower, center: @labels.k}, false
    render.addArc {from: lo_v.lower, to: lo_v.upper, center: @labels.i}, false
    render.addArc {from: ro_v.upper, to: gk_v.upper, center: @labels.k}, false

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawH: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @wide
    ll_brs = ll_br.towards @labels.d, @serif

    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @wide
    ll_trs = ll_tr.towards @labels.b, @serif

    outline.drawLine ll_bl, ll_tl
    outline.drawLine ll_br, ll_tr
    lbls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl
    ltls = outline.drawTouchingCircle ll_bl, ll_tl, @labels.a
    ltrs = outline.drawTouchingCircle ll_trs, ll_tr, ll_br
    lbrs = outline.drawTouchingCircle ll_tr, ll_br, ll_brs

    rl_br = @labels.d.towards @labels.c, @serif
    rl_bl = rl_br.towards @labels.c, @wide
    rl_bls = rl_bl.towards @labels.c, @serif

    rl_tr = @labels.b.towards @labels.a, @serif
    rl_tl = rl_tr.towards @labels.a, @wide
    rl_tls = rl_tl.towards @labels.a, @serif

    outline.drawLine rl_tr, rl_br
    outline.drawLine rl_tl, rl_bl

    rbls = outline.drawTouchingCircle rl_bls, rl_bl, rl_tl
    rtls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tls
    rtrs = outline.drawTouchingCircle @labels.b, rl_tr, rl_br
    rbrs = outline.drawTouchingCircle rl_tr, rl_br, @labels.d

    ml_l = @labels.e.towards @labels.a, @narrow
    ml_r = @labels.f.towards @labels.b, @narrow
    outline.drawLine ml_l, ml_r

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.addArc lbls, true
    render.addArc ltls, true
    render.addArc ltrs, true
    render.lineTo (intersect ml_l, ml_r, ll_tr, ll_br)
    render.lineTo (intersect ml_l, ml_r, rl_tl, rl_bl)
    render.addArc rtls, true
    render.addArc rtrs, true
    render.addArc rbrs, true
    render.addArc rbls, true
    render.lineTo (intersect @labels.e, @labels.f, rl_tl, rl_bl)
    render.lineTo (intersect @labels.e, @labels.f, ll_tr, ll_br)
    render.addArc lbrs, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawI: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    ab = midPoint @labels.a, @labels.b
    cd = midPoint @labels.c, @labels.d

    bl = cd.towards @labels.c, @wide / 2
    bll = bl.towards @labels.c, @serif

    br = cd.towards @labels.d, @wide / 2
    brr = br.towards @labels.d, @serif

    tl = ab.towards @labels.a, @wide / 2
    tll = tl.towards @labels.a, @serif

    tr = ab.towards @labels.b, @wide / 2
    trr = tr.towards @labels.b, @serif

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
    outline.labelPoints @labels
    return

  drawK: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @wide
    ll_brr = ll_br.towards @labels.d, @serif

    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @wide
    ll_trr = ll_tr.towards @labels.b, @serif

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    lls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl
    uls = outline.drawTouchingCircle ll_bl, ll_tl, @labels.a
    urs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br
    lrs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    rul_l = intersect @labels.e, @labels.f, ll_tr, ll_br
    rul_r = intersect @labels.a, @labels.b, rul_l, (new Point rul_l.x + @labels.b.x - @labels.c.x, rul_l.y + @labels.b.y - @labels.c.y)
    outline.drawLine rul_l, rul_r
    rul_rr = rul_r.towards @labels.b, @serif
    rul_rs = outline.drawTouchingCircle rul_rr, rul_r, rul_l

    temp = rul_l.towards (new Point rul_l.x + @labels.a.x - @labels.d.x, rul_l.y + @labels.a.y - @labels.d.y), @narrow
    rul_ll = intersect ll_tr, ll_br, temp, (new Point temp.x + @labels.b.x - @labels.c.x, temp.y + @labels.b.y - @labels.c.y)
    rul_rl = intersect @labels.a, @labels.b, temp, rul_ll
    outline.drawLine rul_ll, rul_rl
    rul_rll = rul_rl.towards @labels.a, @serif 
    rul_ls = outline.drawTouchingCircle rul_ll, rul_rl, rul_rll

    rll_lr = intersect @labels.c, @labels.d, rul_ll, (new Point rul_ll.x + @labels.d.x - @labels.a.x, rul_ll.y + @labels.d.y - @labels.a.y)
    rll_ll = intersect rul_l, rul_r, rul_ll, rll_lr
    outline.drawLine rll_lr, rll_ll

    rll_lt = outline.drawTouchingCircle @labels.d, rll_lr, rll_ll
    @labels.g = rll_lt.center

    rll_ul = rll_ll.towards rul_r, @wide
    rll_ur = intersect @labels.c, @labels.d, rll_ul, (new Point rll_ul.x + @labels.d.x - @labels.a.x, rll_ul.y + @labels.d.y - @labels.a.y)
    outline.drawLine rll_ur, rll_ul

    rll_ut = outline.drawTouchingCircle rll_ul, rll_ur, @labels.d
    @labels.h = rll_ut.center

    render = new FillShape sys.ctx
    render.moveTo @labels.c
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
    outline.labelPoints @labels
    return

  drawM: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    dab = @labels.a.distance @labels.b
    @labels.f = @labels.a.towards @labels.b, dab / 6
    @labels.g = @labels.b.towards @labels.a, dab / 6

    ll_tl = @labels.f.towards @labels.a, @narrow
    outline.drawLine ll_tl, @labels.c
    ll_br = @labels.c.towards @labels.d, @narrow
    outline.drawLine @labels.f, ll_br
    ll_bll = @labels.c.towards @labels.d, -@serif
    ll_brr = ll_br.towards @labels.d, @serif
    ll_ls = outline.drawTouchingCircle ll_bll, @labels.c, ll_tl
    ll_rs = outline.drawTouchingCircle @labels.f, ll_br, ll_brr
    ll_ts = outline.drawTouchingCircle @labels.c, ll_tl, (@labels.a.towards @labels.b, -@narrow)

    rl_tr = @labels.g.towards @labels.b, @narrow
    outline.drawLine rl_tr, @labels.d
    rl_tl = rl_tr.towards @labels.a, @wide
    rl_bl = @labels.d.towards @labels.c, @wide
    outline.drawLine rl_tl, rl_bl
    rl_bll = rl_bl.towards @labels.c, @serif
    rl_ls = outline.drawTouchingCircle rl_bll, rl_bl, rl_tl
    rl_brr = @labels.d.towards @labels.c, -@serif
    rl_rs = outline.drawTouchingCircle rl_tr, @labels.d, rl_brr
    rl_ts = outline.drawTouchingCircle (@labels.b.towards @labels.a, @narrow), rl_tr, @labels.d

    @labels.e = midPoint @labels.c, @labels.d # ll_br, rl_bl

    ml_lt = @labels.f.towards @labels.a, @wide
    outline.drawLine ml_lt, @labels.e
    ml_rb = @labels.e.towards @labels.d, @wide
    outline.drawLine @labels.f, ml_rb

    mr_lb = @labels.e.towards @labels.c, @narrow
    outline.drawLine @labels.g, mr_lb
    mr_rt = @labels.g.towards @labels.b, @narrow
    outline.drawLine mr_rt, @labels.e

    render = new FillShape sys.ctx
    render.moveTo @labels.c
    render.addArc ll_ls, true
    render.addArc ll_ts, true
    render.lineTo @labels.f
    render.lineTo (intersect @labels.f, ml_rb, mr_lb, @labels.g)
    render.lineTo @labels.g
    render.addArc rl_ts, true
    render.addArc rl_rs, true
    render.addArc rl_ls, true
    render.lineTo (intersect rl_bl, rl_tl, @labels.e, mr_rt)
    render.lineTo @labels.e
    render.lineTo (intersect @labels.e, ml_lt, @labels.f, ll_br)
    render.addArc ll_rs, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawN: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    ll_bl = @labels.c.towards @labels.d, @serif
    ll_br = ll_bl.towards @labels.d, @narrow
    ll_brr = ll_br.towards @labels.d, @serif
    ll_tl = @labels.a.towards @labels.b, @serif
    ll_tr = ll_tl.towards @labels.b, @narrow

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    ll_ls = outline.drawTouchingCircle @labels.c, ll_bl, ll_tl
    ll_ts = outline.drawTouchingCircle ll_bl, ll_tl, (@labels.a.towards @labels.b, -@serif / (Math.sqrt 2))
    ll_rs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    @labels.e = @labels.d.towards @labels.c, @serif
    outline.drawLine @labels.a, @labels.e
    outline.drawLine ll_tr, @labels.d
    rl_bl = @labels.e.towards @labels.c, @narrow
    rl_tr = @labels.b.towards @labels.a, @serif
    rl_tl = rl_tr.towards @labels.a, @narrow
    rl_tll = rl_tl.towards @labels.a, @serif

    outline.drawLine rl_tl, rl_bl
    outline.drawLine rl_tr, @labels.e

    rl_ls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tll
    rl_rs = outline.drawTouchingCircle @labels.b, rl_tr, @labels.e

    render = new FillShape sys.ctx
    render.moveTo @labels.c
    render.addArc ll_ls, true
    render.addArc ll_ts, true
    render.lineTo ll_tr
    render.lineTo (intersect ll_tr, @labels.d, rl_bl, rl_tl)
    render.addArc rl_ls, true
    render.addArc rl_rs, true
    render.lineTo @labels.e
    render.lineTo (intersect @labels.e, @labels.a, ll_tr, ll_br)
    render.addArc ll_rs, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawO: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    outline.drawLine @labels.c, @labels.b

    @labels.e = midPoint @labels.c, @labels.b
    @labels.f = @labels.e.towards @labels.c, @wide / 2
    @labels.g = @labels.e.towards @labels.b, @wide / 2

    ol = new Point @labels.c.x, @labels.f.y
    ob = new Point @labels.f.x, @labels.c.y
    _or = new Point @labels.b.x, @labels.g.y
    ot = new Point @labels.g.x, @labels.b.y

    radius = @labels.f.distance ol

    outline.drawCircle @labels.f, ob
    outline.drawCircle @labels.g, ot

    fgr = new Point @labels.f.y, @labels.g.x
    outline.drawCircle fgr, ot

    fgl = new Point @labels.f.x, @labels.g.y
    outline.drawCircle fgl, ob

    il = new Point @labels.g.x - radius, @labels.g.y
    it = new Point @labels.f.x, @labels.f.y - radius
    ib = new Point @labels.g.x, @labels.g.y + radius
    ir = new Point @labels.f.x + radius, @labels.f.y

    outline.drawCircle fgl, il
    outline.drawCircle fgr, ir

    if variant == 'q'
      outline.drawLine @labels.a, @labels.d
      tailRadius = @labels.c.distance @labels.d
      outline.drawLine @labels.d, (@labels.d.towards @labels.c, -tailRadius)
      @labels.h = new Point (@labels.d.towards @labels.c, -3 * tailRadius / 4).x, @labels.d.y + 0.01

      utc = (intersectCircles @labels.d, tailRadius, @labels.h, tailRadius).left
      outline.drawLine @labels.h, utc
      outline.drawLine @labels.d, utc
      outline.drawCircle utc, @labels.d

      llor = fgl.distance _or
      utci = (intersectCircles utc, tailRadius, fgl, llor).left
      outline.drawCircle utci, (new Point utci.x, utci.y + @wide)
      ltci = (intersectCircles utci, @wide, fgl, llor).left
      ltc = (intersectCircles ltci, tailRadius, @labels.h, tailRadius).left
      outline.drawLine ltci, ltc
      outline.drawLine @labels.h, ltc
      outline.drawCircle ltc, @labels.h

    render = new FillShape sys.ctx

    render.moveTo ob
    render.addArc {from: ob, to: ol, center: @labels.f}, false
    render.addArc {from: ol, to: ot, center: fgr}, false
    render.addArc {from: ot, to: _or, center: @labels.g}, false
    if variant != 'q'
      render.addArc({from: _or, to: ob, center: fgl}, false)
    else
      render.addArc {from: _or, to: utci, center: fgl}, false
      render.addArc {from: utci, to: @labels.h, center: utc}, true
      render.addArc {from: @labels.h, to: ltci, center: ltc}, false
      render.addArc {from: ltci, to: ob, center: fgl}, false

    render.moveTo ib
    render.addArc {from: ib, to: ir, center: fgr}, true
    render.addArc {from: ir, to: it, center: @labels.f}, true
    render.addArc {from: it, to: il, center: fgl}, true
    render.addArc {from: il, to: ib, center: @labels.g}, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawS: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    @labels.g = midPoint @labels.a, @labels.b
    @labels.h = midPoint @labels.c, @labels.d
    outline.drawLine @labels.g, @labels.h

    @labels.m = intersect @labels.e, @labels.f, @labels.g, @labels.h

    @labels.i = @labels.g.towards @labels.h, @narrow
    @labels.k = @labels.h.towards @labels.g, @narrow

    @labels.l = @labels.m.towards @labels.h, @wide / 3
    @labels.n = @labels.m.towards @labels.g, 2 * @wide / 3

    toc = midPoint @labels.g, @labels.l
    outline.drawCircle toc, @labels.g
    tic = midPoint @labels.i, @labels.n
    outline.drawCircle tic, @labels.i

    boc = midPoint @labels.n, @labels.h
    outline.drawCircle boc, @labels.h

    bic = midPoint @labels.l, @labels.k
    outline.drawCircle bic, @labels.k

    rtl = new Point tic.x + (tic.distance @labels.i), tic.y
    outline.drawCircle rtl, (rtl.towards tic, 4 * @wide / 3)
    rtu = (intersectCircles rtl, 4 * @wide / 3, toc, (toc.distance @labels.g)).left
    outline.drawLine rtl, rtu

    bir = bic.distance @labels.l
    bor = boc.distance @labels.n
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
    render.addArc {from: lti, to: @labels.l, center: bic}, true
    render.addArc {from: @labels.l, to: rtu, center: toc}, false
    render.addArc {from: rtl, to: @labels.n, center: tic}, true
    render.addArc {from: @labels.n, to: lt.lower, center: boc}, false

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawT: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    l_bl = (midPoint @labels.c, @labels.d).towards @labels.c, @wide / 2
    l_br = l_bl.towards @labels.d, @wide
    if variant != 'j'
      l_bll = l_bl.towards @labels.c, @serif
      l_brr = l_br.towards @labels.d, @serif

    l_tl = (midPoint @labels.a, @labels.b).towards @labels.a, @wide / 2
    l_tr = l_tl.towards @labels.b, @wide

    @labels.e = @labels.e = @labels.a.towards @labels.b, @wide
    @labels.f = @labels.f = @labels.b.towards @labels.a, @wide

    outline.drawLine l_tl, l_bl
    outline.drawLine l_tr, l_br
    if variant != 'j'
      bls = outline.drawTouchingCircle l_bll, l_bl, l_tl
      brs = outline.drawTouchingCircle l_tr, l_br, l_brr
    else
      lob = midPoint @labels.c, l_br
      lor = lob.distance @labels.c
      loc = new Point @labels.c.x + lor, @labels.c.y - lor
      outline.drawCircle loc, lob

      lilb = new Point @labels.e.x - @narrow / 2, @labels.c.y
      lib = midPoint lilb, l_bl
      lir = lib.distance l_bl
      lic = new Point lib.x, @labels.c.y - lir - @narrow
      outline.drawCircle lic, (new Point lib.x, lib.y - @narrow)
      outline.drawLine @labels.e, (new Point @labels.e.x, @labels.c.y)

      ltu = (vertCircle lic, lir, @labels.e.x).lower
      ltl = (vertCircle loc, lor, @labels.e.x).lower

    tl_l = @labels.a.towards @labels.c, @narrow
    tl_r = @labels.b.towards @labels.d, @narrow
    outline.drawLine tl_l, tl_r

    tl_ll = new Point @labels.e.x - 3 * @narrow, @labels.e.y + 2 * @narrow
    tl_lu = new Point @labels.e.x + @narrow, @labels.e.y - @narrow
    outline.drawLine tl_ll, tl_lu

    tl_rl = new Point @labels.f.x - 2 * @narrow, @labels.e.y + 2 * @narrow
    tl_ru = new Point @labels.f.x + 2 * @narrow, @labels.e.y - @narrow
    outline.drawLine tl_rl, tl_ru

    tlls = outline.drawTouchingCircle tl_r, (intersect tl_l, tl_r, tl_ll, tl_lu), tl_ll
    tlus = outline.drawTouchingCircle tl_lu, (intersect @labels.a, @labels.b, tl_ll, tl_lu), @labels.b

    trus = outline.drawTouchingCircle @labels.a, (intersect @labels.a, @labels.b, tl_rl, tl_ru), tl_ru
    trls = outline.drawTouchingCircle tl_rl, (intersect tl_l, tl_r, tl_rl, tl_ru), tl_l

    render = new FillShape sys.ctx

    if variant != 'j'
      render.moveTo bls.from
      render.addArc bls, true
    else
      render.moveTo ltl
      render.addArc {from: ltu, to:(new Point l_bl.x, lic.y), center: lic}, true

    render.lineTo (intersect l_bl, l_tl, tl_l, tl_r)
    render.addArc tlls, true
    render.addArc tlus, true
    render.addArc trus, true
    render.addArc trls, true
    render.lineTo (intersect l_br, l_tr, tl_l, tl_r)
    if variant != 'j'
      render.addArc brs, true
    else
      render.addArc {from:(new Point l_br.x, loc.y), to: ltl, center: loc}, false

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawU: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.c, @labels.d
    @labels.f = @labels.a.towards @labels.b, @serif
    @labels.g = @labels.c.towards @labels.d, @serif
    outline.drawLine @labels.f, @labels.g

    @labels.h = @labels.b.towards @labels.a, @serif
    @labels.i = @labels.d.towards @labels.c, @serif
    outline.drawLine @labels.h, @labels.i

    ll_tr = @labels.f.towards @labels.b, @wide
    ll_trr = ll_tr.towards @labels.b, @serif
    ll_br = @labels.g.towards @labels.d, @wide
    outline.drawLine ll_tr, ll_br
    tlls = outline.drawTouchingCircle @labels.g, @labels.f, @labels.a
    tlrs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br

    rl_tl = @labels.h.towards @labels.a, @wide
    rl_tll = rl_tl.towards @labels.a, @serif
    rl_bl = @labels.i.towards @labels.c, @wide
    outline.drawLine rl_tl, rl_bl
    rlls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tll
    rlrs = outline.drawTouchingCircle @labels.b, @labels.h, @labels.i

    lor = @labels.e.distance @labels.g
    loc = new Point @labels.e.x, @labels.e.y - lor
    outline.drawCircle loc, @labels.e

    lir = @labels.e.distance ll_br
    lic = new Point @labels.e.x, @labels.e.y - lir - @narrow
    outline.drawCircle lic, (new Point @labels.e.x, @labels.e.y - @narrow)

    render = new FillShape sys.ctx

    render.moveTo tlls.from
    render.addArc tlls, true
    render.addArc tlrs, true
    render.addArc {from:(new Point ll_br.x, lic.y), to:(new Point rl_bl.x, lic.y), center: lic}, true
    render.addArc rlls, true
    render.addArc rlrs, true
    render.addArc {from:(new Point @labels.h.x, loc.y), to:(new Point @labels.f.x, loc.y), center: loc}, false

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawV: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.c, @labels.d
    @labels.f = @labels.a.towards @labels.b, @serif
    @labels.g = @labels.b.towards @labels.a, @serif

    ll_tr = @labels.f.towards @labels.b, @wide
    ll_trr = ll_tr.towards @labels.b, @serif
    ll_br = @labels.e.towards @labels.d, @wide

    outline.drawLine @labels.f, @labels.e
    outline.drawLine ll_tr, ll_br
    llls = outline.drawTouchingCircle @labels.e, @labels.f, @labels.a
    llrs = outline.drawTouchingCircle ll_trr, ll_tr, ll_br

    rl_tl = @labels.g.towards @labels.a, @narrow
    rl_tll = rl_tl.towards @labels.a, @serif
    rl_bl = @labels.e.towards @labels.c, @narrow

    outline.drawLine rl_tl, rl_bl
    outline.drawLine @labels.g, @labels.e
    rlls = outline.drawTouchingCircle rl_bl, rl_tl, rl_tll
    rlrs = outline.drawTouchingCircle @labels.b, @labels.g, @labels.e

    render = new FillShape sys.ctx

    render.moveTo @labels.e
    render.addArc llls, true
    render.addArc llrs, true
    render.lineTo (intersect ll_tr, ll_br, rl_tl, rl_bl)
    render.addArc rlls, true
    render.addArc rlrs, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawW: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.b
    fg = midPoint @labels.c, @labels.d
    @labels.f = midPoint @labels.c, fg
    @labels.g = midPoint fg, @labels.d

    f_l = @labels.f.towards @labels.c, @narrow
    f_r = @labels.f.towards @labels.d, @wide

    g_l = @labels.g.towards @labels.c, @narrow
    g_r = @labels.g.towards @labels.d, @wide

    a_l = @labels.a.towards @labels.b, -@serif
    a_r = @labels.a.towards @labels.b, @wide
    a_rr = a_r.towards @labels.b, @serif

    outline.drawLine @labels.a, @labels.f
    outline.drawLine a_r, f_r

    e_l = @labels.e.towards @labels.a, @wide
    e_r = @labels.e.towards @labels.b, @narrow

    outline.drawLine @labels.e, f_l
    outline.drawLine e_r, @labels.f
    als = outline.drawTouchingCircle @labels.f, @labels.a, a_l
    ars = outline.drawTouchingCircle a_rr, a_r, f_r

    outline.drawLine e_l, @labels.g
    outline.drawLine @labels.e, g_r

    b_l = @labels.b.towards @labels.a, @narrow
    b_ll = b_l.towards @labels.a, @serif
    b_r = @labels.b.towards @labels.a, -@serif

    outline.drawLine b_l, g_l
    outline.drawLine @labels.b, @labels.g
    bls = outline.drawTouchingCircle g_l, b_l, b_ll
    brs = outline.drawTouchingCircle b_r, @labels.b, @labels.g

    render = new FillShape sys.ctx

    render.moveTo @labels.f
    render.addArc als, true
    render.addArc ars, true
    render.lineTo (intersect a_r, f_r, f_l, @labels.e)
    render.lineTo @labels.e
    render.lineTo (intersect @labels.e, g_r, g_l, b_l)
    render.addArc bls, true
    render.addArc brs, true
    render.lineTo @labels.g
    render.lineTo (intersect @labels.g, e_l, e_r, @labels.f)

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawX: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.f = @labels.a.towards @labels.b, @serif
    @labels.g = @labels.c.towards @labels.d, @serif
    outline.drawLine @labels.f, @labels.g

    @labels.h = @labels.b.towards @labels.a, @serif
    @labels.i = @labels.d.towards @labels.c, @serif
    outline.drawLine @labels.h, @labels.i

    tl_r = @labels.f.towards @labels.b, @wide
    tl_rr = tl_r.towards @labels.b, @serif

    bl_r = @labels.g.towards @labels.d, @narrow
    bl_rr = bl_r.towards @labels.d, @serif

    tr_l = @labels.h.towards @labels.a, @narrow
    tr_ll = tr_l.towards @labels.a, @serif

    br_l = @labels.i.towards @labels.c, @wide
    br_ll = br_l.towards @labels.c, @serif

    outline.drawLine @labels.f, br_l
    outline.drawLine tl_r, @labels.i

    outline.drawLine tr_l, @labels.g
    outline.drawLine @labels.h, bl_r

    tlls = outline.drawTouchingCircle br_l, @labels.f, @labels.a
    tlrs = outline.drawTouchingCircle tl_rr, tl_r, @labels.i

    trls = outline.drawTouchingCircle @labels.g, tr_l, tr_ll
    trrs = outline.drawTouchingCircle @labels.b, @labels.h, bl_r

    blls = outline.drawTouchingCircle @labels.c, @labels.g, tr_l
    blrs = outline.drawTouchingCircle @labels.h, bl_r, bl_rr

    brrs = outline.drawTouchingCircle tl_r, @labels.i, @labels.d
    brls = outline.drawTouchingCircle br_ll, br_l, @labels.f

    render = new FillShape sys.ctx

    render.moveTo @labels.g
    render.addArc blls, true
    render.lineTo (intersect @labels.g, tr_l, @labels.f, br_l)
    render.addArc tlls, true
    render.addArc tlrs, true
    render.lineTo (intersect tl_r, @labels.i, @labels.g, tr_l)
    render.addArc trls, true
    render.addArc trrs, true
    render.lineTo (intersect @labels.h, bl_r, @labels.i, tl_r)
    render.addArc brrs, true
    render.addArc brls, true
    render.lineTo (intersect br_l, @labels.f, @labels.h, bl_r)
    render.addArc blrs, true

    render.closeAndFill()
    outline.labelPoints @labels
    return

  drawY: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = midPoint @labels.a, @labels.c
    @labels.f = midPoint @labels.b, @labels.d
    outline.drawLine @labels.e, @labels.f

    ll_bl = (midPoint @labels.c, @labels.d).towards @labels.c, @wide / 2
    ll_bll = ll_bl.towards @labels.c, @serif
    ll_br = ll_bl.towards @labels.d, @wide
    ll_brr = ll_br.towards @labels.d, @serif

    ll_tl = (midPoint @labels.e, @labels.f).towards @labels.e, @wide / 2
    ll_tr = ll_tl.towards @labels.f, @wide

    outline.drawLine ll_tl, ll_bl
    outline.drawLine ll_tr, ll_br

    bls = outline.drawTouchingCircle ll_bll, ll_bl, ll_tl
    brs = outline.drawTouchingCircle ll_tr, ll_br, ll_brr

    tlm = ll_tl.towards ll_tr, 2 * @wide / 3

    tl_l = @labels.a.towards @labels.b, @serif
    tl_r = tl_l.towards @labels.b, 2 * @wide / 3
    tl_rr = tl_r.towards @labels.b, @serif

    tr_r = @labels.b.towards @labels.a, @serif
    tr_l = tr_r.towards @labels.a, @wide / 3
    tr_ll = tr_l.towards @labels.a, @serif

    outline.drawLine tl_l, ll_tl
    outline.drawLine tl_r, tlm

    outline.drawLine tr_l, tlm
    outline.drawLine tr_r, ll_tr

    tlls = outline.drawTouchingCircle ll_tl, tl_l, @labels.a
    tlrs = outline.drawTouchingCircle tl_rr, tl_r, tlm

    trls = outline.drawTouchingCircle tlm, tr_l, tr_ll
    trrs = outline.drawTouchingCircle @labels.b, tr_r, ll_tr

    render = new FillShape sys.ctx

    render.moveTo (midPoint @labels.c, @labels.d)
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
    outline.labelPoints @labels
    return

  drawZ: (id, variant, proportions) ->
    outline = new OutlineDrawer sys.ctx

    outline.drawLine @labels.a, @labels.b
    outline.drawLine @labels.c, @labels.d
    outline.drawLine @labels.a, @labels.c
    outline.drawLine @labels.b, @labels.d

    @labels.e = @labels.a.towards @labels.c, @serif
    @labels.f = @labels.a.towards @labels.b, @serif
    outline.drawLine @labels.e, @labels.f

    @labels.g = @labels.d.towards @labels.c, @serif
    @labels.h = @labels.d.towards @labels.b, @serif
    outline.drawLine @labels.g, @labels.h

    tl_l = @labels.a.towards @labels.c, @narrow
    tl_r = @labels.b.towards @labels.d, @narrow
    outline.drawLine tl_l, tl_r
    tls = outline.drawTouchingCircle tl_r, (intersect tl_l, tl_r, @labels.e, @labels.f), @labels.e

    bl_l = @labels.c.towards @labels.a, @narrow
    bl_r = @labels.d.towards @labels.b, @narrow
    outline.drawLine bl_l, bl_r
    brs = outline.drawTouchingCircle bl_l, (intersect bl_l, bl_r, @labels.g, @labels.h), @labels.h

    ml_br = @labels.c.towards @labels.d, @wide * 1.25
    ml_tl = @labels.b.towards @labels.a, @wide * 1.25

    outline.drawLine ml_tl, @labels.c
    outline.drawLine @labels.b, ml_br

    render = new FillShape sys.ctx

    render.moveTo @labels.c
    render.lineTo (intersect @labels.c, ml_tl, tl_l, tl_r)
    render.addArc tls, true
    render.lineTo @labels.f
    render.lineTo @labels.b
    render.lineTo (intersect @labels.b, ml_br, bl_l, bl_r)
    render.addArc brs, true
    render.lineTo @labels.g

    render.closeAndFill()
    outline.labelPoints @labels
    return

  lookupGlyphFunction: (letter) ->
    switch letter
      when 'a' then 'drawA'
      when 'b' then 'drawB'
      when 'c' then 'drawC'
      when 'd' then 'drawD'
      when 'e' then 'drawE'
      when 'f' then 'drawE'
      when 'g' then 'drawG'
      when 'h' then 'drawH'
      when 'i' then 'drawI'
      when 'j' then 'drawT'
      when 'k' then 'drawK'
      when 'l' then 'drawE'
      when 'm' then 'drawM'
      when 'n' then 'drawN'
      when 'o' then 'drawO'
      when 'p' then 'drawB'
      when 'q' then 'drawO'
      when 'r' then 'drawB'
      when 's' then 'drawS'
      when 't' then 'drawT'
      when 'u' then 'drawU'
      when 'v' then 'drawV'
      when 'w' then 'drawW'
      when 'x' then 'drawX'
      when 'y' then 'drawY'
      when 'z' then 'drawZ'
      else undefined

  getGlyphFunction: (letter) ->
    t = this
    fname = @lookupGlyphFunction letter
    if fname != undefined
      return (id, variant, prop) =>
        @initializeGlyph id, prop
        @[fname] id, variant, prop
    else
      return fname

  drawLetter: (id, letter, proportions) ->
    letter = letter.toLowerCase()

    f = @getGlyphFunction letter
    if f != undefined
      f id, letter, proportions
      # catch e then console.log 'Exception', e, 'drawing', letter
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
    f = GlyphFunctions.getGlyphFunction letter

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
    kernOffset = [0, 0, 0, 0, 0]

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
