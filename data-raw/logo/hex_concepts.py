"""Generate the lemur hex logo as SVG.

Concept: the app itself - a monitor (the "life expectancy monitor") showing the
lemur dashboard: a title bar and four high-contrast panels - the lemur, a
region map, the life expectancy headline (e0) and the gain band (life-years
gained under a "what if?" mortality change).

Colorways: hex-lemur-dark (black field, white monitor) and hex-lemur-light
(ice field, black monitor). Palette: saturated yellow / cyan / magenta / green
panel blocks, black-on-color graphics, no tints.

Run: python data-raw/logo/hex_concepts.py
Then rasterize the 1024 px slots in render_hires.html to man/figures/*.png
(the Edge/Chrome command is printed at the end), or open render_hires.html
in a browser.
"""
import math

W, H = 512, 591
HEX = [(256, 0), (512, 147.8), (512, 443.4), (256, 591), (0, 443.4), (0, 147.8)]

DARK_TOP, DARK_BOT = "#16161D", "#050508"
ICE_TOP, ICE_BOT = "#F0F0F4", "#D9D9E0"
BLACK = "#14141C"
PAPER = "#FAFAF5"
YELLOW = "#FFD23F"
CYAN = "#29C7F6"
MAGENTA = "#FF2E63"
GREEN = "#2FD980"
AMBER = "#FF9330"

SANS = "Segoe UI Variable Display, Segoe UI, Helvetica Neue, Arial, sans-serif"


def wrap(inner, bg_top, bg_bot, grad_id):
    hexpts = " ".join(f"{x},{y}" for x, y in HEX)
    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
  <defs>
    <linearGradient id="{grad_id}" x1="0" y1="0" x2="0.35" y2="1">
      <stop offset="0" stop-color="{bg_top}"/>
      <stop offset="1" stop-color="{bg_bot}"/>
    </linearGradient>
    <clipPath id="hexclip"><polygon points="{hexpts}"/></clipPath>
  </defs>
  <polygon points="{hexpts}" fill="url(#{grad_id})"/>
  <g clip-path="url(#hexclip)">
{inner}
  </g>
</svg>
"""


def wordmark(strong, light):
    return (
        f'<text x="256" y="492" text-anchor="middle" font-family="{SANS}" '
        f'font-size="64" letter-spacing="0.5" fill="{strong}">'
        f'<tspan font-weight="650">lemur</tspan></text>\n'
        f'    <text x="256" y="522" text-anchor="middle" font-family="{SANS}" '
        f'font-size="12" letter-spacing="2.5" font-weight="300" fill="{light}">'
        f'LIFE EXPECTANCY MONITOR</text>'
    )


def card(x0, y0, x1, y1, col):
    return (f'<rect x="{x0}" y="{y0}" width="{x1 - x0}" height="{y1 - y0}" '
            f'rx="8" fill="{col}"/>')


# ---------------------------------------------------------------- panels

def lemur_panel():
    """Top-left: the lemur - banded tail, white face, amber eyes."""
    tail = "M 212,228 C 236,224 244,198 234,170"
    return f"""<g>
      <path d="{tail}" fill="none" stroke="{PAPER}" stroke-width="7.5" stroke-linecap="round"/>
      <path d="{tail}" fill="none" stroke="{BLACK}" stroke-width="7.5" stroke-dasharray="5 5"/>
      <circle cx="152" cy="172" r="9" fill="{BLACK}"/>
      <circle cx="198" cy="172" r="9" fill="{BLACK}"/>
      <circle cx="152" cy="172" r="4" fill="{PAPER}"/>
      <circle cx="198" cy="172" r="4" fill="{PAPER}"/>
      <circle cx="175" cy="192" r="30" fill="{BLACK}"/>
      <ellipse cx="175" cy="197" rx="21" ry="22" fill="{PAPER}"/>
      <path d="M 155,188 A 21,22 0 0 1 195,188 Q 175,194 155,188 Z" fill="{BLACK}"/>
      <g transform="rotate(-18 167 197)"><ellipse cx="167" cy="197" rx="6.5" ry="8.5" fill="{BLACK}"/></g>
      <g transform="rotate(18 183 197)"><ellipse cx="183" cy="197" rx="6.5" ry="8.5" fill="{BLACK}"/></g>
      <circle cx="167" cy="198" r="3.8" fill="{AMBER}"/>
      <circle cx="183" cy="198" r="3.8" fill="{AMBER}"/>
      <circle cx="167" cy="198" r="1.6" fill="{BLACK}"/>
      <circle cx="183" cy="198" r="1.6" fill="{BLACK}"/>
      <path d="M 170,208 L 180,208 L 175,214 Z" fill="{BLACK}" stroke="{BLACK}" stroke-width="3" stroke-linejoin="round"/>
    </g>"""


def map_panel():
    """Top-right: region map - landmasses on ocean, one pinned region."""
    blobs = (
        "M 272,182 Q 282,172 296,176 Q 310,180 306,194 Q 312,204 300,212 "
        "Q 284,216 276,206 Q 266,196 272,182 Z",
        "M 318,168 Q 332,162 346,168 Q 354,174 348,184 Q 336,190 324,184 "
        "Q 312,178 318,168 Z",
        "M 360,188 Q 376,180 392,186 Q 404,192 398,204 Q 388,214 372,210 "
        "Q 356,204 360,188 Z",
    )
    shapes = "\n      ".join(
        f'<path d="{d}" fill="{BLACK}"/>' for d in blobs)
    islands = "\n      ".join(
        f'<circle cx="{x}" cy="{y}" r="{r}" fill="{BLACK}"/>'
        for x, y, r in ((316, 208, 3), (352, 214, 2.5), (398, 176, 2.5)))
    return f"""<g>
      {shapes}
      {islands}
      <circle cx="292" cy="187" r="6.5" fill="{MAGENTA}"/>
      <path d="M 286,191 L 298,191 L 292,204 Z" fill="{MAGENTA}" stroke="{MAGENTA}" stroke-width="3" stroke-linejoin="round"/>
      <circle cx="292" cy="187" r="2.5" fill="{PAPER}"/>
    </g>"""


def ex_panel():
    """Bottom-left: the headline life expectancy number and an e(x) sketch."""
    return f"""<g>
      <text x="114" y="266" font-family="{SANS}" font-size="15" font-weight="600" fill="{PAPER}" opacity="0.9">e&#8320;</text>
      <text x="114" y="306" font-family="{SANS}" font-size="40" font-weight="650" fill="{PAPER}">73.2</text>
      <path d="M 210,276 L 222,284 L 232,280 L 244,292" fill="none" stroke="{PAPER}" stroke-width="3.5" stroke-linecap="round" stroke-linejoin="round"/>
      <circle cx="244" cy="292" r="3.5" fill="{PAPER}"/>
    </g>"""


def surv(x):
    return 1.0 / (1.0 + math.exp((x - 50.0) / 9.5))


def improved(x):
    return 1.0 / (1.0 + math.exp((x - 85.0) / 9.5))


def mini_curve(fn, x0, x1, ybase, span, n=81):
    return [(x0 + (x1 - x0) * i / (n - 1), ybase - span * fn(100.0 * i / (n - 1)))
            for i in range(n)]


def path_of(pts):
    return "M " + " L ".join(f"{px:.2f},{py:.2f}" for px, py in pts)


def gain_panel():
    """Bottom-right: baseline vs improved survivorship, band = years gained."""
    x0, x1, ybase, span = 274, 400, 304, 46
    base = mini_curve(lambda x: 0.92 * surv(x), x0, x1, ybase, span)
    imp = mini_curve(improved, x0, x1, ybase, span)
    band = (path_of(imp) + " L " +
            " L ".join(f"{px:.2f},{py:.2f}" for px, py in reversed(base)) + " Z")
    return f"""<g>
      <path d="{band}" fill="{PAPER}"/>
      <path d="{path_of(base)}" fill="none" stroke="{BLACK}" stroke-width="3.5" stroke-linecap="round" stroke-dasharray="7 5"/>
      <path d="{path_of(imp)}" fill="none" stroke="{BLACK}" stroke-width="4.5" stroke-linecap="round"/>
      <text x="402" y="264" text-anchor="end" font-family="{SANS}" font-size="15" font-weight="650" fill="{BLACK}">+2.4</text>
    </g>"""


# ---------------------------------------------------------------- dashboard

def dashboard(body, screen, divider):
    return f"""<rect x="70" y="114" width="372" height="238" rx="22" fill="{body}"/>
    <rect x="90" y="136" width="332" height="202" rx="12" fill="{screen}"/>
    <circle cx="106" cy="149" r="5" fill="{MAGENTA}"/>
    <circle cx="122" cy="149" r="5" fill="{YELLOW}"/>
    <circle cx="138" cy="149" r="5" fill="{CYAN}"/>
    <rect x="154" y="144" width="76" height="10" rx="5" fill="{divider}"/>
    <line x1="90" y1="163" x2="422" y2="163" stroke="{divider}" stroke-width="2"/>
    <circle cx="406" cy="345" r="3" fill="{CYAN}"/>
    <rect x="232" y="352" width="48" height="38" rx="6" fill="{body}"/>
    <rect x="186" y="384" width="140" height="14" rx="7" fill="{body}"/>
    {card(98, 170, 252, 245, YELLOW)}
    {card(260, 170, 414, 245, CYAN)}
    {card(98, 253, 252, 328, MAGENTA)}
    {card(260, 253, 414, 328, GREEN)}
    <g transform="translate(0 12)">
    {lemur_panel()}
    {map_panel()}
    {ex_panel()}
    {gain_panel()}
    </g>"""


def hex_dark():
    inner = dashboard("#F2F2EC", "#101018", "#2A2A38") + f"\n    {wordmark(PAPER, YELLOW)}"
    return wrap(inner, DARK_TOP, DARK_BOT, "dark")


def hex_light():
    inner = dashboard(BLACK, "#FAFAF5", "#DDDDE4") + f"\n    {wordmark(BLACK, MAGENTA)}"
    return wrap(inner, ICE_TOP, ICE_BOT, "ice")


CONCEPTS = {
    "hex-lemur-dark": hex_dark,
    "hex-lemur-light": hex_light,
}

if __name__ == "__main__":
    for name, fn in CONCEPTS.items():
        with open(f"data-raw/logo/{name}.svg", "w", encoding="utf-8") as f:
            f.write(fn())
        print(f"wrote data-raw/logo/{name}.svg")
    print("rasterize: msedge --headless=new --default-background-color=00000000 "
          "--window-size=1024,1182 --screenshot=<out.png> <slot.html>")
