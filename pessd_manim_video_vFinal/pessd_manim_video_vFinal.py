from manim import *
import os
import textwrap

# ============================================================
# PESSD VIDEO FINAL 
# Timothée Dangleterre - Nathan Granier - Giovanni Manche
# Project in Economics, Sociology and Data Science
#
# Run:
#   manim -pqh pessd_manim_video_final.py PESSDVideoFinal
#
# ============================================================

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
IMAGE_DIR = os.path.join(BASE_DIR, "images")
AUDIO_DIR = os.path.join(BASE_DIR, "scripts")


def image_path(filename):
    return os.path.join(IMAGE_DIR, filename)


def audio_path(filename):
    return os.path.join(AUDIO_DIR, filename)


config.pixel_width = 1920
config.pixel_height = 1080
config.frame_rate = 30
config.background_color = "#F6F9FC"

# Colors
NAVY = "#0B2D4D"
BLUE = "#2563EB"
TEAL = "#059669"
GREEN = "#16A34A"
RED = "#DC2626"
ORANGE = "#F59E0B"
PURPLE = "#7C3AED"
GRAY = "#475569"
LIGHT_GRAY = "#E2E8F0"
MID_GRAY = "#CBD5E1"
WHITE = "#FFFFFF"
BLACK = "#0F172A"

TITLE_FONT = "DejaVu Serif"
BODY_FONT = "DejaVu Serif"
SANS_FONT = "DejaVu Sans"


def safe_text(text, size=36, color=BLACK, font=BODY_FONT, weight=None, slant=None, max_width=None):
    """Create a Text object and shrink it if needed."""
    kwargs = {"font": font, "font_size": size, "color": color}
    if weight is not None:
        kwargs["weight"] = weight
    if slant is not None:
        kwargs["slant"] = slant
    t = Text(text, **kwargs)
    if max_width is not None and t.width > max_width:
        t.scale_to_fit_width(max_width)
    return t


def para(text, size=32, color=GRAY, width=10.0, font=BODY_FONT, line_spacing=0.78):
    """Multi-line text with safe wrapping."""
    lines = []
    max_chars = max(18, int(width * 8.5 * 32 / max(size, 1)))
    for raw in text.split("\n"):
        lines.extend(textwrap.wrap(raw, width=max_chars) if raw.strip() else [""])
    out = Text("\n".join(lines), font=font, font_size=size, color=color, line_spacing=line_spacing)
    if out.width > width:
        out.scale_to_fit_width(width)
    return out


def rounded_box(width, height, stroke=NAVY, fill=WHITE, fill_opacity=1.0, radius=0.18, stroke_width=3):
    return RoundedRectangle(
        width=width,
        height=height,
        corner_radius=radius,
        stroke_color=stroke,
        stroke_width=stroke_width,
        fill_color=fill,
        fill_opacity=fill_opacity,
    )


def flag(country, scale=0.55):
    """Small flag made only of vector shapes: DE, FR, IT, ES."""
    w, h = 1.35 * scale, 0.82 * scale
    border = RoundedRectangle(
        width=w,
        height=h,
        corner_radius=0.03,
        stroke_color=MID_GRAY,
        stroke_width=1.2,
        fill_opacity=0,
    )
    g = VGroup(border)

    if country == "DE":
        colors = ["#000000", "#DD0000", "#FFCE00"]
        for i, c in enumerate(colors):
            r = Rectangle(width=w, height=h / 3, stroke_width=0, fill_color=c, fill_opacity=1)
            r.move_to(border.get_center() + UP * (h / 3 - i * h / 3))
            g.add(r)

    elif country == "FR":
        colors = ["#0055A4", "#FFFFFF", "#EF4135"]
        for i, c in enumerate(colors):
            r = Rectangle(width=w / 3, height=h, stroke_width=0, fill_color=c, fill_opacity=1)
            r.move_to(border.get_center() + LEFT * (w / 3 - i * w / 3))
            g.add(r)

    elif country == "IT":
        colors = ["#009246", "#FFFFFF", "#CE2B37"]
        for i, c in enumerate(colors):
            r = Rectangle(width=w / 3, height=h, stroke_width=0, fill_color=c, fill_opacity=1)
            r.move_to(border.get_center() + LEFT * (w / 3 - i * w / 3))
            g.add(r)

    elif country == "ES":
        heights = [h * 0.25, h * 0.5, h * 0.25]
        colors = ["#AA151B", "#F1BF00", "#AA151B"]
        y = h / 2
        for hh, c in zip(heights, colors):
            r = Rectangle(width=w, height=hh, stroke_width=0, fill_color=c, fill_opacity=1)
            r.move_to(border.get_center() + UP * (y - hh / 2))
            y -= hh
            g.add(r)

    g.add(border.copy().set_z_index(4))
    return g


def pdf_to_png(pdf_path, out_dir=".manim_fig_cache", dpi=190):
    """Convert first page of PDF to PNG. Requires pymupdf. Falls back cleanly if missing."""
    if not os.path.exists(pdf_path):
        return None

    os.makedirs(out_dir, exist_ok=True)
    base = os.path.splitext(os.path.basename(pdf_path))[0]
    out = os.path.join(out_dir, f"{base}_{dpi}.png")

    if os.path.exists(out):
        return out

    try:
        import fitz

        doc = fitz.open(pdf_path)
        page = doc[0]
        zoom = dpi / 72
        pix = page.get_pixmap(matrix=fitz.Matrix(zoom, zoom), alpha=False)
        pix.save(out)
        doc.close()
        return out
    except Exception as e:
        print(f"Could not convert {pdf_path}: {e}")
        return None


def graph_card(pdf_name, title=None, max_w=12.6, max_h=6.65):
    """A clean large figure card. Uses ImageMobject when the PDF is available."""
    png = pdf_to_png(pdf_name)

    if png:
        img = ImageMobject(png)
        if img.width > max_w:
            img.scale_to_fit_width(max_w)
        if img.height > max_h:
            img.scale_to_fit_height(max_h)
        card = rounded_box(
            img.width + 0.35,
            img.height + 0.35,
            stroke=LIGHT_GRAY,
            stroke_width=2,
            radius=0.16,
        )
        img.move_to(card.get_center())
        grp = Group(card, img)
    else:
        card = rounded_box(max_w, 4.7, stroke=LIGHT_GRAY, stroke_width=2, radius=0.16)
        missing = para(f"Missing figure:\n{pdf_name}", size=30, color=RED, width=max_w - 1)
        missing.move_to(card.get_center())
        grp = VGroup(card, missing)

    if title:
        tt = safe_text(title, size=26, color=NAVY, font=BODY_FONT, weight=BOLD, max_width=max_w)
        tt.next_to(grp, UP, buff=0.12)
        return Group(tt, grp)

    return grp


def bullet(text, color=GRAY, size=30, width=5.5):
    dot = Circle(radius=0.055, fill_color=color, fill_opacity=1, stroke_width=0)
    tx = para(text, size=size, color=GRAY, width=width)
    g = VGroup(dot, tx).arrange(RIGHT, buff=0.18, aligned_edge=UP)
    return g


class PESSDVideoFinal(Scene):
    def setup(self):
        self.slide_no = 0
        self._wait_scale = 1.0
        self._timed_scene_deadline = None

    def wait(self, duration=DEFAULT_WAIT_TIME, stop_condition=None, frozen_frame=None):
        return super().wait(
            duration * getattr(self, "_wait_scale", 1.0),
            stop_condition=stop_condition,
            frozen_frame=frozen_frame,
        )

    def current_time(self):
        return float(getattr(self.renderer, "time", getattr(self, "time", 0.0)))

    def add_audio_if_exists(self, audio_file):
        if audio_file and os.path.exists(audio_file):
            self.add_sound(audio_file)
        elif audio_file:
            print(f"[Audio warning] Missing audio file: {audio_file}")

    def run_synced_section(self, audio_file, target_duration, scene_functions, wait_scale=1.0):
        self.add_audio_if_exists(audio_file)
        start = self.current_time()
        previous_scale = self._wait_scale
        self._wait_scale = wait_scale

        for fn in scene_functions:
            fn()

        self._wait_scale = previous_scale
        elapsed = self.current_time() - start
        remaining = target_duration - elapsed

        if remaining > 0:
            super().wait(remaining)
        else:
            print(f"[Timing warning] Section {audio_file or 'visual'} is {abs(remaining):.2f}s longer than target.")

    def run_timed_scene(self, scene_function, target_duration, wait_scale=1.0):
        start = self.current_time()
        previous_scale = self._wait_scale
        previous_deadline = self._timed_scene_deadline

        self._wait_scale = wait_scale
        self._timed_scene_deadline = start + target_duration

        scene_function()

        self._wait_scale = previous_scale
        remaining = self._timed_scene_deadline - self.current_time()

        if remaining > 0:
            super().wait(remaining)
        elif remaining < -0.20:
            print(f"[Timing warning] Scene {scene_function.__name__} is {abs(remaining):.2f}s longer than its target.")

        self._timed_scene_deadline = previous_deadline

    def run_synced_scene_plan(self, audio_file, scene_plan):
        self.add_audio_if_exists(audio_file)

        for scene_function, target_duration, wait_scale in scene_plan:
            self.run_timed_scene(scene_function, target_duration, wait_scale)

    def clear_slide(self, keep=None):
        if keep is None:
            keep = []

        deadline = getattr(self, "_timed_scene_deadline", None)
        if deadline is not None:
            remaining = deadline - self.current_time()
            if remaining > 0:
                super().wait(remaining)

        mobs = [m for m in self.mobjects if m not in keep]
        if mobs:
            self.play(FadeOut(*mobs, shift=DOWN * 0.05), run_time=0.45)

    def header(self, title, subtitle=None):
        self.slide_no += 1

        badge = Circle(radius=0.27, fill_color=NAVY, fill_opacity=1, stroke_width=0)
        num = safe_text(str(self.slide_no), size=24, color=WHITE, font=SANS_FONT, weight=BOLD)
        num.move_to(badge)
        badge_grp = VGroup(badge, num).to_corner(UL, buff=0.35)

        title_txt = safe_text(title, size=42, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=11.5)
        title_txt.next_to(badge_grp, RIGHT, buff=0.22).align_to(badge_grp, UP).shift(DOWN * 0.02)

        line = Line(LEFT * 6.9, RIGHT * 6.9, stroke_color=LIGHT_GRAY, stroke_width=2).to_edge(UP, buff=0.96)
        elems = VGroup(badge_grp, title_txt, line)

        if subtitle:
            sub = safe_text(subtitle, size=22, color=GRAY, font=BODY_FONT, max_width=10.8)
            sub.next_to(title_txt, DOWN, buff=0.12).align_to(title_txt, LEFT)
            elems.add(sub)

        self.play(FadeIn(badge_grp, scale=0.9), Write(title_txt), Create(line), run_time=0.9)

        if subtitle:
            self.play(FadeIn(sub, shift=UP * 0.05), run_time=0.35)

        return elems

    def footer(self):
        return VGroup()

    def title_scene(self):
        title = para(
            "Real-Time Fiscal Fragmentation\nand Monetary Policy Transmission",
            size=52,
            color=NAVY,
            width=11.8,
            font=TITLE_FONT,
            line_spacing=0.72,
        )
        title.to_edge(UP, buff=1.0)

        authors = safe_text(
            "Timothée Dangleterre · Nathan Granier · Giovanni Manche",
            size=28,
            color=GRAY,
            font=BODY_FONT,
            max_width=11,
        )

        course = para(
            "Project in Economics, Sociology and Data Science",
            size=26,
            color=TEAL,
            width=10.5,
            font=BODY_FONT,
        )

        authors.next_to(title, DOWN, buff=0.45)
        course.next_to(authors, DOWN, buff=0.18)

        flags = VGroup()
        for c in ["DE", "FR", "IT", "ES"]:
            fg = flag(c, scale=0.70)
            lbl = safe_text(c, size=22, color=NAVY, font=SANS_FONT, weight=BOLD)
            flags.add(VGroup(fg, lbl).arrange(DOWN, buff=0.10))

        flags.arrange(RIGHT, buff=0.55).next_to(course, DOWN, buff=0.65)

        rule = Line(LEFT * 4.8, RIGHT * 4.8, stroke_color=LIGHT_GRAY, stroke_width=2)
        rule.next_to(flags, DOWN, buff=0.55)

        tag = safe_text("Euro area · fiscal news · monetary transmission", size=23, color=GRAY, font=BODY_FONT)
        tag.next_to(rule, DOWN, buff=0.26)

        self.play(FadeIn(title, shift=DOWN * 0.15), run_time=0.8)
        self.play(FadeIn(authors), FadeIn(course), run_time=0.5)
        self.play(
            LaggedStart(*[FadeIn(c, shift=UP * 0.12) for c in flags], lag_ratio=0.10),
            Create(rule),
            FadeIn(tag),
            run_time=1.0,
        )
        self.wait(6.25)
        self.clear_slide()

    def intro_scene(self):
        self.header("Research question")

        left = rounded_box(4.35, 2.65, stroke=BLUE, fill="#EFF6FF")
        right = rounded_box(4.35, 2.65, stroke=TEAL, fill="#ECFDF5")
        left.move_to(LEFT * 3.15 + UP * 0.65)
        right.move_to(RIGHT * 3.15 + UP * 0.65)

        ecb = safe_text("One monetary policy", size=32, color=BLUE, font=TITLE_FONT, weight=BOLD, max_width=3.8)
        ecb_sub = para("Interest rates are set for the whole euro area.", size=23, color=GRAY, width=3.55)
        ecb_group = VGroup(ecb, ecb_sub).arrange(DOWN, buff=0.24).move_to(left)

        flags = VGroup(*[flag(c, scale=0.43) for c in ["DE", "FR", "IT", "ES"]]).arrange(RIGHT, buff=0.16)
        fisc = safe_text("Many fiscal policies", size=32, color=TEAL, font=TITLE_FONT, weight=BOLD, max_width=3.8)
        fisc_sub = para("Taxes, spending and fiscal risks remain national.", size=23, color=GRAY, width=3.55)
        fisc_group = VGroup(flags, fisc, fisc_sub).arrange(DOWN, buff=0.20).move_to(right)

        qbox = rounded_box(11.2, 1.45, stroke=NAVY, fill=WHITE)
        q = para(
            "Does fiscal heterogeneity affect how monetary policy is transmitted across countries?",
            size=30,
            color=NAVY,
            width=10.4,
            font=TITLE_FONT,
        )
        q.move_to(qbox)
        qgrp = VGroup(qbox, q).to_edge(DOWN, buff=0.95)

        connector = Arrow(UP * 0.05, qgrp.get_top() + UP * 0.13, buff=0.05, color=NAVY, stroke_width=5)

        self.play(FadeIn(left), FadeIn(ecb_group, shift=UP * 0.1), run_time=0.8)
        self.wait(3.2)
        self.play(FadeIn(right), FadeIn(fisc_group, shift=UP * 0.1), run_time=0.8)
        self.wait(3.2)
        self.play(GrowArrow(connector), FadeIn(qgrp, shift=UP * 0.16), run_time=0.8)
        self.wait(6.0)
        self.clear_slide()

    def literature_scene(self):
        self.header("How we position the project")

        intro = para(
            "The existing literature already studies financial fragmentation and macroeconomic nowcasting. "
            "Our project connects these two strands in a new way.",
            size=27,
            color=GRAY,
            width=11.6,
            font=BODY_FONT,
        )
        intro.to_edge(UP, buff=1.25)

        left = rounded_box(5.35, 4.15, stroke=BLUE, fill="#EFF6FF")
        right = rounded_box(5.35, 4.15, stroke=TEAL, fill="#ECFDF5")
        left.move_to(LEFT * 3.1 + DOWN * 0.15)
        right.move_to(RIGHT * 3.1 + DOWN * 0.15)

        lt = safe_text("What is usually done", size=30, color=BLUE, font=TITLE_FONT, weight=BOLD, max_width=4.8)
        rt = safe_text("What we add", size=30, color=TEAL, font=TITLE_FONT, weight=BOLD, max_width=4.8)
        lt.next_to(left.get_top(), DOWN, buff=0.30)
        rt.next_to(right.get_top(), DOWN, buff=0.30)

        old_items = VGroup(
            bullet("Fragmentation is often measured through sovereign spreads.", color=BLUE, size=22, width=4.25),
            bullet("Nowcasting is mainly used to forecast macroeconomic variables.", color=BLUE, size=22, width=4.25),
            bullet("Fiscal data are rarely available in real time.", color=BLUE, size=22, width=4.25),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.28)
        old_items.move_to(left.get_center() + DOWN * 0.25)

        new_items = VGroup(
            bullet("We build a real-time fiscal fragmentation index.", color=TEAL, size=22, width=4.25),
            bullet("The index is based on nowcast revisions, not market prices.", color=TEAL, size=22, width=4.25),
            bullet("We apply it to ECB monetary policy transmission.", color=TEAL, size=22, width=4.25),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.28)
        new_items.move_to(right.get_center() + DOWN * 0.25)

        arrow = Arrow(left.get_right() + RIGHT * 0.10, right.get_left() + LEFT * 0.10, color=NAVY, stroke_width=6, buff=0.15)
        label = rounded_box(3.8, 0.68, stroke=NAVY, fill=WHITE)
        label_t = safe_text("from measurement to transmission", size=19, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=3.45)
        label_g = VGroup(label, label_t.move_to(label)).move_to(DOWN * 2.72)

        self.play(FadeIn(intro, shift=DOWN * 0.08), run_time=0.6)
        self.play(FadeIn(left), FadeIn(lt), run_time=0.55)
        self.play(LaggedStart(*[FadeIn(i, shift=RIGHT * 0.12) for i in old_items], lag_ratio=0.18), run_time=1.1)
        self.wait(2.8)
        self.play(GrowArrow(arrow), FadeIn(label_g, shift=UP * 0.08), run_time=0.8)
        self.play(FadeIn(right), FadeIn(rt), run_time=0.55)
        self.play(LaggedStart(*[FadeIn(i, shift=LEFT * 0.12) for i in new_items], lag_ratio=0.18), run_time=1.1)
        self.wait(5.5)
        self.clear_slide()

    def nowcast_concept_scene(self):
        self.header("Step 1: fiscal nowcasting")
        f = self.footer()
        self.add(f)

        inputs = VGroup()
        for name, sub in [
            ("Employment", "labour market"),
            ("Production", "business cycle"),
            ("Prices", "inflation and energy"),
        ]:
            b = rounded_box(3.1, 0.92, stroke=GRAY, fill=WHITE)
            t = safe_text(name, size=28, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=2.6)
            s = safe_text(sub, size=19, color=GRAY, font=BODY_FONT, max_width=2.7)
            g = VGroup(b, VGroup(t, s).arrange(DOWN, buff=0.06).move_to(b)).arrange()
            inputs.add(g)

        inputs.arrange(DOWN, buff=0.28).move_to(LEFT * 4.65 + DOWN * 0.2)

        now = rounded_box(3.25, 1.65, stroke=BLUE, fill="#DBEAFE")
        now_t = safe_text("NOWCAST", size=34, color=BLUE, font=TITLE_FONT, weight=BOLD)
        now_s = para("updated after each\nnew release", size=22, color=GRAY, width=2.5)
        now_group = VGroup(now, VGroup(now_t, now_s).arrange(DOWN, buff=0.16).move_to(now)).move_to(DOWN * 0.1)

        out = rounded_box(3.45, 1.65, stroke=TEAL, fill=WHITE)
        formula = safe_text("D̂ = R̂ − Ê", size=34, color=NAVY, font=TITLE_FONT, weight=BOLD)
        rev = safe_text("new estimate", size=24, color=TEAL, font=BODY_FONT, weight=BOLD)
        out_group = VGroup(out, VGroup(formula, rev).arrange(DOWN, buff=0.18).move_to(out)).move_to(RIGHT * 4.55 + DOWN * 0.1)

        arr1 = Arrow(inputs.get_right() + RIGHT * 0.15, now_group.get_left() + LEFT * 0.1, buff=0.05, color=BLUE, stroke_width=6)
        arr2 = Arrow(now_group.get_right() + RIGHT * 0.15, out_group.get_left() + LEFT * 0.1, buff=0.05, color=TEAL, stroke_width=6)

        small_eq = rounded_box(5.0, 0.75, stroke=NAVY, fill=WHITE)
        small_eq_t = safe_text("Public deficit = revenues − expenditures", size=25, color=NAVY, font=BODY_FONT, weight=BOLD, max_width=4.6)
        small_eq_g = VGroup(small_eq, small_eq_t.move_to(small_eq)).to_edge(UP, buff=1.2)

        self.play(FadeIn(small_eq_g, shift=DOWN * 0.1), run_time=0.7)
        self.wait(2.8)
        self.play(LaggedStart(*[FadeIn(i, shift=RIGHT * 0.15) for i in inputs], lag_ratio=0.16), run_time=1.2)
        self.wait(3.0)
        self.play(GrowArrow(arr1), FadeIn(now_group, scale=0.95), run_time=1.0)
        self.wait(3.2)
        self.play(GrowArrow(arr2), FadeIn(out_group, shift=LEFT * 0.15), run_time=1.0)
        self.wait(3.2)

        pulse = Circle(radius=0.15, color=TEAL).move_to(out_group.get_center() + DOWN * 0.46 + RIGHT * 0.6)
        self.play(GrowFromCenter(pulse), pulse.animate.scale(4).set_opacity(0), run_time=0.9)
        self.wait(3.2)
        self.clear_slide()

    def ragged_edge_scene(self):
        self.header("Pseudo real-time information flow")

        cap = para(
            "At each date, the model only uses information that would have been available to policymakers. "
            "Because indicators are released at different frequencies and with different publication delays, the dataset has a ragged edge.",
            size=24,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.22)

        cal_box = rounded_box(5.25, 4.25, stroke=BLUE, fill="#EFF6FF")
        cal_box.move_to(LEFT * 3.25 + DOWN * 0.20)
        cal_title = safe_text("Sequential releases", size=28, color=BLUE, font=TITLE_FONT, weight=BOLD)
        cal_title.next_to(cal_box.get_top(), DOWN, buff=0.22)

        timeline = Line(LEFT * 1.85, RIGHT * 1.85, stroke_color=BLUE, stroke_width=5).move_to(cal_box.get_center() + DOWN * 0.15)

        months = VGroup()
        for i, m in enumerate(["t", "t+15", "t+40", "t+75"]):
            x = -1.85 + i * (3.70 / 3)
            dot = Dot(radius=0.075, color=BLUE).move_to(timeline.get_center() + RIGHT * x)
            lab = safe_text(m, size=18, color=GRAY, font=SANS_FONT).next_to(dot, DOWN, buff=0.16)
            months.add(VGroup(dot, lab))

        release_items = [
            ("Inflation", -1.70, 1.20, RED),
            ("Unemployment", -0.55, 0.72, TEAL),
            ("Production", 0.55, 1.20, BLUE),
            ("Fiscal data", 1.70, 0.72, ORANGE),
        ]

        rels = VGroup()
        for name, x, y, col in release_items:
            b = rounded_box(1.34, 0.42, stroke=col, fill=WHITE, stroke_width=2, radius=0.09)
            t = safe_text(name, size=13, color=col, font=BODY_FONT, weight=BOLD, max_width=1.12)
            g = VGroup(b, t.move_to(b)).move_to(timeline.get_center() + RIGHT * x + UP * y)
            target = timeline.get_center() + RIGHT * x + UP * 0.14
            arr = Arrow(g.get_bottom() + DOWN * 0.01, target, color=col, stroke_width=2.4, buff=0.04, max_tip_length_to_length_ratio=0.18)
            rels.add(VGroup(g, arr))

        rag_box = rounded_box(5.25, 4.25, stroke=TEAL, fill="#ECFDF5")
        rag_box.move_to(RIGHT * 3.25 + DOWN * 0.20)
        rag_title = safe_text("Ragged-edge vintage", size=28, color=TEAL, font=TITLE_FONT, weight=BOLD)
        rag_title.next_to(rag_box.get_top(), DOWN, buff=0.22)

        grid = VGroup()
        for r in range(6):
            row = VGroup()
            for c in range(8):
                available = c < 8 - max(0, r - 1)
                cell = Square(
                    side_length=0.30,
                    stroke_color=WHITE,
                    stroke_width=1.2,
                    fill_color=TEAL if available else LIGHT_GRAY,
                    fill_opacity=0.85 if available else 0.35,
                )
                row.add(cell)
            row.arrange(RIGHT, buff=0.03)
            grid.add(row)

        grid.arrange(DOWN, buff=0.04).move_to(rag_box.get_center() + DOWN * 0.05)

        row_labs = VGroup()
        for k, lab in enumerate(["R", "E", "IP", "HICP", "Unemp.", "Cash"]):
            tx = safe_text(lab, size=13, color=GRAY, font=SANS_FONT, max_width=0.7)
            tx.next_to(grid[k], LEFT, buff=0.12)
            row_labs.add(tx)

        arrow = Arrow(cal_box.get_right() + RIGHT * 0.10, rag_box.get_left() + LEFT * 0.10, color=NAVY, stroke_width=6, buff=0.15)

        nowcast = rounded_box(5.7, 0.82, stroke=NAVY, fill=WHITE)
        nowcast_t = safe_text("Updated nowcast → fiscal surprise", size=25, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=5.1)
        nowcast_g = VGroup(nowcast, nowcast_t.move_to(nowcast)).to_edge(DOWN, buff=0.64)

        arr_down = Arrow(rag_box.get_bottom() + DOWN * 0.10, nowcast_g.get_top() + UP * 0.08, color=NAVY, stroke_width=5, buff=0.05)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(cal_box), FadeIn(cal_title), Create(timeline), FadeIn(months), run_time=0.9)
        self.play(LaggedStart(*[FadeIn(x, shift=DOWN * 0.08) for x in rels], lag_ratio=0.18), run_time=1.2)
        self.wait(3.0)
        self.play(GrowArrow(arrow), FadeIn(rag_box), FadeIn(rag_title), run_time=0.8)
        self.play(LaggedStart(*[FadeIn(row) for row in grid], lag_ratio=0.08), FadeIn(row_labs), run_time=1.1)
        self.wait(3.0)
        self.play(GrowArrow(arr_down), FadeIn(nowcast_g, shift=UP * 0.10), run_time=0.8)
        self.wait(6.0)
        self.clear_slide()

    def surprises_concept_scene(self):
        self.header("Step 2: fiscal surprises")
        f = self.footer()
        self.add(f)

        eq_box = rounded_box(5.2, 0.9, stroke=RED, fill=WHITE)
        eq = safe_text("Surprise = new nowcast − old nowcast", size=27, color=RED, font=TITLE_FONT, weight=BOLD, max_width=4.8)
        eq_group = VGroup(eq_box, eq.move_to(eq_box)).to_edge(UP, buff=1.25)

        old = rounded_box(3.2, 1.3, stroke=GRAY, fill=WHITE)
        old_t = safe_text("Old nowcast", size=28, color=GRAY, font=TITLE_FONT, weight=BOLD)
        old_v = safe_text("before new data", size=22, color=GRAY, font=BODY_FONT)
        old_g = VGroup(old, VGroup(old_t, old_v).arrange(DOWN, buff=0.12).move_to(old)).move_to(LEFT * 3.6)

        new = rounded_box(3.2, 1.3, stroke=TEAL, fill=WHITE)
        new_t = safe_text("New nowcast", size=28, color=TEAL, font=TITLE_FONT, weight=BOLD)
        new_v = safe_text("after new data", size=22, color=GRAY, font=BODY_FONT)
        new_g = VGroup(new, VGroup(new_t, new_v).arrange(DOWN, buff=0.12).move_to(new)).move_to(RIGHT * 3.6)

        arrow = Arrow(old_g.get_right() + RIGHT * 0.2, new_g.get_left() + LEFT * 0.2, buff=0.05, color=RED, stroke_width=6)

        lab = rounded_box(4.05, 0.68, stroke=RED, fill="#FEF2F2")
        lab_t = safe_text("revision = surprise", size=22, color=RED, font=TITLE_FONT, weight=BOLD, max_width=3.65)
        lab_g = VGroup(lab, lab_t.move_to(lab)).next_to(arrow, DOWN, buff=0.25)

        bars = VGroup()
        for label, col in [("positive revision", TEAL), ("negative revision", RED)]:
            b = rounded_box(3.4, 0.62, stroke=col, fill=WHITE)
            t = safe_text(label, size=23, color=col, font=TITLE_FONT, weight=BOLD)
            bars.add(VGroup(b, t.move_to(b)))

        bars.arrange(DOWN, buff=0.18).to_edge(DOWN, buff=0.95)

        self.play(FadeIn(eq_group, shift=DOWN * 0.1), run_time=0.6)
        self.wait(3.2)
        self.play(FadeIn(old_g, shift=RIGHT * 0.15), FadeIn(new_g, shift=LEFT * 0.15), run_time=0.8)
        self.wait(3.2)
        self.play(GrowArrow(arrow), FadeIn(lab_g, shift=UP * 0.1), run_time=0.8)
        self.wait(3.2)
        self.play(LaggedStart(*[FadeIn(b, shift=UP * 0.1) for b in bars], lag_ratio=0.18), run_time=0.8)
        self.wait(3.2)
        self.clear_slide()

    def surprises_graph_scene(self):
        self.header("Raw fiscal surprises: revisions over time")

        cap = para(
            "Each bar is one nowcast update. Large positive or negative bars correspond to large fiscal news arriving in real time.",
            size=24,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.25)

        fig = graph_card(image_path("raw_surprises_total_factor_midas.pdf"), max_w=12.75, max_h=6.05)
        fig.next_to(cap, DOWN, buff=0.22)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(fig, scale=0.99), run_time=0.9)
        self.wait(14.0)
        self.clear_slide()

    def fragmentation_concept_scene(self):
        self.header("Step 3: from surprises to fragmentation")

        subtitle = para(
            "Fragmentation rises when fiscal surprises diverge across countries.",
            size=31,
            color=NAVY,
            width=12.0,
            font=TITLE_FONT,
        )
        subtitle.to_edge(UP, buff=1.22)

        def mini_bars(vals, title, color, pos):
            box = rounded_box(4.15, 3.25, stroke=LIGHT_GRAY, fill=WHITE)
            box.move_to(pos)

            title_t = safe_text(title, size=25, color=color, font=TITLE_FONT, weight=BOLD, max_width=3.7)
            title_t.next_to(box.get_top(), DOWN, buff=0.22)

            base = Line(LEFT * 1.35, RIGHT * 1.35, color=LIGHT_GRAY, stroke_width=3)
            base.move_to(box.get_center() + DOWN * 0.75)

            bars = VGroup()
            for i, v in enumerate(vals):
                r = Rectangle(width=0.34, height=v, stroke_width=0, fill_color=color, fill_opacity=0.78)
                r.move_to(base.get_center() + LEFT * 1.0 + RIGHT * i * 0.66 + UP * v / 2)

                lab = safe_text(["DE", "FR", "IT", "ES"][i], size=15, color=GRAY, font=SANS_FONT)
                lab.move_to(base.get_center() + LEFT * 1.0 + RIGHT * i * 0.66 + DOWN * 0.25)

                bars.add(VGroup(r, lab))

            return VGroup(box, title_t, base, bars)

        low = mini_bars([1.05, 1.16, 1.00, 1.10], "Low dispersion", TEAL, LEFT * 3.25 + DOWN * 0.40)
        high = mini_bars([0.20, 1.18, 0.62, 1.34], "High dispersion", RED, RIGHT * 3.25 + DOWN * 0.40)

        result = rounded_box(5.4, 0.78, stroke=NAVY, fill=WHITE)
        result_t = safe_text("Fiscal fragmentation index", size=23, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=4.8)
        result_g = VGroup(result, result_t.move_to(result)).to_edge(DOWN, buff=0.68)

        arr = Arrow(UP * (-0.2), result_g.get_top() + UP * 0.1, color=NAVY, stroke_width=5)

        self.play(FadeIn(subtitle, shift=DOWN * 0.05), run_time=0.5)
        self.play(FadeIn(low, shift=UP * 0.1), run_time=0.8)
        self.wait(4.0)
        self.play(FadeIn(high, shift=UP * 0.1), run_time=0.8)
        self.wait(3.2)
        self.play(GrowArrow(arr), FadeIn(result_g, shift=UP * 0.12), run_time=0.7)
        self.wait(6.0)
        self.clear_slide()

    def fragmentation_graph_scene(self):
        self.header("Result: a real-time fiscal fragmentation index")
        f = self.footer()
        self.add(f)

        cap = para(
            "The index is built from the cross-country dispersion of fiscal surprises. Higher values mean that countries receive more divergent fiscal news in real time.",
            size=24,
            color=GRAY,
            width=11.6,
        )
        cap.to_edge(UP, buff=1.25)

        fig = graph_card(image_path("euro_fragmentation_index_total.pdf"), max_w=12.5, max_h=5.95)
        fig.next_to(cap, DOWN, buff=0.22)

        self.play(FadeIn(cap), run_time=0.5)
        self.play(FadeIn(fig, scale=0.985), run_time=1.0)
        self.wait(14.0)
        self.clear_slide()

    def contributions_graph_scene(self):
        self.header("Which countries drive fragmentation?")

        cap = para(
            "The bars decompose the contribution of each country to the euro-area fragmentation measure. This shows not only when fragmentation rises, but also where it comes from.",
            size=23,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.25)

        fig = graph_card(image_path("contributions_fragmentation_total_factor_midas.pdf"), max_w=12.75, max_h=6.0)
        fig.next_to(cap, DOWN, buff=0.20)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(fig, scale=0.99), run_time=0.9)
        self.wait(14.0)
        self.clear_slide()

    def monetary_policy_scene(self):
        self.header("Step 4: monetary policy transmission")

        main = para(
            "We test whether monetary policy shocks have different effects when fiscal fragmentation is high.",
            size=29,
            color=NAVY,
            width=11.0,
            font=TITLE_FONT,
        )
        main.to_edge(UP, buff=1.22)

        items = [
            ("ECB announcement", "high-frequency financial reaction", BLUE),
            ("Monetary policy shock", "unexpected change around announcement", PURPLE),
            ("Fiscal fragmentation", "state variable measured in real time", RED),
        ]

        cards = VGroup()
        for title, sub, col in items:
            b = rounded_box(3.45, 1.18, stroke=col, fill=WHITE)
            t = safe_text(title, size=22, color=col, font=TITLE_FONT, weight=BOLD, max_width=3.05)
            s = para(sub, size=17, color=GRAY, width=2.95)
            cards.add(VGroup(b, VGroup(t, s).arrange(DOWN, buff=0.08).move_to(b)))

        cards.arrange(RIGHT, buff=0.38).move_to(UP * 0.55)

        arrows = VGroup(
            Arrow(cards[0].get_right() + RIGHT * 0.06, cards[1].get_left() + LEFT * 0.06, buff=0, color=GRAY, stroke_width=4),
            Arrow(cards[1].get_right() + RIGHT * 0.06, cards[2].get_left() + LEFT * 0.06, buff=0, color=GRAY, stroke_width=4),
        )

        response = rounded_box(5.0, 1.16, stroke=TEAL, fill="#ECFDF5")
        rt = safe_text("Macroeconomic response", size=25, color=TEAL, font=TITLE_FONT, weight=BOLD, max_width=4.45)
        rs = safe_text("output, industrial production, activity", size=19, color=GRAY, font=BODY_FONT, max_width=4.25)
        response_g = VGroup(response, VGroup(rt, rs).arrange(DOWN, buff=0.08).move_to(response)).move_to(DOWN * 1.60)

        arr_down = Arrow(cards[2].get_bottom() + DOWN * 0.08, response_g.get_top() + UP * 0.08, color=TEAL, stroke_width=5)

        eq = rounded_box(7.8, 0.72, stroke=GRAY, fill=WHITE, fill_opacity=0.95)
        eq_t = safe_text("Effect = β × shock  +  γ × shock × fragmentation", size=24, color=NAVY, font=TITLE_FONT, max_width=7.2)
        eq_g = VGroup(eq, eq_t.move_to(eq)).to_edge(DOWN, buff=0.68)

        self.play(FadeIn(main, shift=DOWN * 0.05), run_time=0.6)
        self.wait(4.0)
        self.play(LaggedStart(*[FadeIn(c, shift=UP * 0.1) for c in cards], lag_ratio=0.18), run_time=1.0)
        self.play(Create(arrows), run_time=0.55)
        self.wait(3.2)
        self.play(GrowArrow(arr_down), FadeIn(response_g, shift=UP * 0.12), run_time=0.75)
        self.wait(3.2)
        self.play(FadeIn(eq_g, shift=UP * 0.1), run_time=0.65)
        self.wait(4.0)
        self.clear_slide()

    def mps_graph_scene(self):
        self.header("Identifying ECB monetary policy shocks")

        cap = para(
            "We use high-frequency financial surprises around ECB announcements. The series isolates unexpected target and path components of monetary policy.",
            size=23,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.25)

        fig = graph_card(image_path("mps_series_euro_area.pdf"), max_w=12.5, max_h=5.95)
        fig.next_to(cap, DOWN, buff=0.22)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(fig, scale=0.99), run_time=0.9)
        self.wait(10.0)
        self.clear_slide()

    def static_regression_results_scene(self):
        self.header("Main empirical result: static interaction")

        cap = para(
            "We estimate whether the effect of a monetary policy surprise depends on the level of fiscal fragmentation. "
            "The interaction coefficient is the key parameter.",
            size=23,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.25)

        left = rounded_box(5.55, 4.65, stroke=NAVY, fill=WHITE)
        right = rounded_box(6.20, 4.65, stroke=LIGHT_GRAY, fill=WHITE)
        left.move_to(LEFT * 3.25 + DOWN * 0.30)
        right.move_to(RIGHT * 3.05 + DOWN * 0.30)

        eq_title = safe_text("Static regression", size=28, color=NAVY, font=TITLE_FONT, weight=BOLD)
        eq_title.next_to(left.get_top(), DOWN, buff=0.24)

        eq = MathTex(
            r"\Delta Y_t = \alpha + \beta MPS_t + \gamma(MPS_t \times Frag_t) + \delta Frag_t + X_t'\theta + \varepsilon_t",
            font_size=30,
            color=BLACK,
        )
        eq.scale_to_fit_width(4.95)
        eq.next_to(eq_title, DOWN, buff=0.45)

        gamma_box = rounded_box(4.45, 1.18, stroke=RED, fill="#FEF2F2")
        gamma_box.next_to(eq, DOWN, buff=0.55)

        gamma_title = safe_text("Coefficient of interest", size=21, color=GRAY, font=BODY_FONT)
        gamma_val = safe_text("γ = −2.17***", size=34, color=RED, font=TITLE_FONT, weight=BOLD)
        gamma_g = VGroup(gamma_title, gamma_val).arrange(DOWN, buff=0.07).move_to(gamma_box)
        gamma_all = VGroup(gamma_box, gamma_g)

        interp = para(
            "Higher fiscal fragmentation makes a restrictive monetary policy shock more contractionary.",
            size=22,
            color=NAVY,
            width=4.7,
            font=TITLE_FONT,
        )
        interp.next_to(gamma_all, DOWN, buff=0.35)

        fig = graph_card(image_path("marginal_effect_gdp_weighted.pdf"), max_w=5.55, max_h=3.45)
        fig.move_to(right.get_center() + DOWN * 0.42)

        fig_title = safe_text("Marginal effect across fiscal states", size=23, color=NAVY, font=TITLE_FONT, weight=BOLD, max_width=5.4)
        fig_title.next_to(right.get_top(), DOWN, buff=0.28)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(left), FadeIn(eq_title), Write(eq), run_time=1.0)
        self.wait(2.0)
        self.play(FadeIn(gamma_all, scale=0.96), run_time=0.7)
        self.play(FadeIn(interp, shift=UP * 0.08), run_time=0.5)
        self.wait(3.5)
        self.play(FadeIn(right), FadeIn(fig_title), FadeIn(fig, scale=0.98), run_time=1.0)
        self.wait(8.0)
        self.clear_slide()

    def irf_results_scene(self):
        self.header("Empirical result: responses depend on fragmentation")

        cap = para(
            "Conditional impulse responses translate the interaction result into dynamics. Under high fragmentation, restrictive monetary policy shocks generate a stronger contractionary response.",
            size=23,
            color=GRAY,
            width=11.8,
        )
        cap.to_edge(UP, buff=1.25)

        fig = graph_card(image_path("irf_conditional_gdp_weighted.pdf"), max_w=12.5, max_h=5.95)
        fig.next_to(cap, DOWN, buff=0.22)

        self.play(FadeIn(cap), run_time=0.45)
        self.play(FadeIn(fig, scale=0.99), run_time=0.9)
        self.wait(12.0)
        self.clear_slide()

    def conclusion_scene_final3s(self):
        scene_start = self.current_time()
        deadline = getattr(self, "_timed_scene_deadline", None)

        if deadline is not None and deadline > scene_start:
            scene_target = deadline - scene_start
        else:
            scene_target = 21.448

        final_only_duration = 3.0
        final_start = scene_start + scene_target - final_only_duration

        self.header("Conclusion and contribution")
        f = self.footer()
        self.add(f)

        box1 = rounded_box(5.3, 1.55, stroke=TEAL, fill=WHITE)
        c1 = safe_text("Contribution 1", size=30, color=TEAL, font=TITLE_FONT, weight=BOLD)
        c1s = para("A real-time fiscal fragmentation index based on nowcast revisions.", size=23, color=GRAY, width=4.7)
        g1 = VGroup(box1, VGroup(c1, c1s).arrange(DOWN, buff=0.14).move_to(box1)).move_to(LEFT * 3.05 + UP * 0.5)

        box2 = rounded_box(5.3, 1.55, stroke=BLUE, fill=WHITE)
        c2 = safe_text("Contribution 2", size=30, color=BLUE, font=TITLE_FONT, weight=BOLD)
        c2s = para("A framework to test heterogeneous monetary policy transmission.", size=23, color=GRAY, width=4.7)
        g2 = VGroup(box2, VGroup(c2, c2s).arrange(DOWN, buff=0.14).move_to(box2)).move_to(RIGHT * 3.05 + UP * 0.5)

        final_box = rounded_box(10.7, 1.35, stroke=NAVY, fill="#EFF6FF")
        final_txt = para(
            "A single monetary policy may face different fiscal states. Real-time measurement helps us understand when transmission becomes uneven.",
            size=25,
            color=NAVY,
            width=9.8,
            font=TITLE_FONT,
        )
        final_g = VGroup(final_box, final_txt.move_to(final_box)).to_edge(DOWN, buff=1.08)

        self.play(FadeIn(g1, shift=UP * 0.1), run_time=0.7)
        self.wait(2.3)
        self.play(FadeIn(g2, shift=UP * 0.1), run_time=0.7)
        self.wait(2.3)
        self.play(FadeIn(final_g, shift=UP * 0.1), run_time=0.8)

        remaining_before_final = final_start - self.current_time()
        if remaining_before_final > 0:
            super().wait(remaining_before_final)

        end = para(
            "Real-time fiscal fragmentation\nmatters for monetary policy transmission",
            size=46,
            color=NAVY,
            width=11.5,
            font=TITLE_FONT,
            line_spacing=0.75,
        )
        end.move_to(UP * 0.35)

        names = safe_text(
            "Timothée Dangleterre · Nathan Granier · Giovanni Manche",
            size=25,
            color=GRAY,
            font=BODY_FONT,
            max_width=11,
        )
        names.next_to(end, DOWN, buff=0.42)

        course = safe_text(
            "Project in Economics, Sociology and Data Science",
            size=22,
            color=TEAL,
            font=BODY_FONT,
            max_width=11,
        )
        course.next_to(names, DOWN, buff=0.16)

        flags = VGroup(*[flag(c, scale=0.42) for c in ["DE", "FR", "IT", "ES"]]).arrange(RIGHT, buff=0.25).next_to(course, DOWN, buff=0.35)

        currently_visible = [m for m in self.mobjects]
        self.play(
            FadeOut(*currently_visible, shift=DOWN * 0.05),
            FadeIn(end, shift=UP * 0.15),
            FadeIn(names),
            FadeIn(course),
            FadeIn(flags),
            run_time=0.55,
        )

        remaining = scene_start + scene_target - self.current_time()
        if remaining > 0:
            super().wait(remaining)

    def construct(self):
        self.run_synced_scene_plan(
            audio_file=audio_path("Giovanni Manche - Script.mp4"),
            scene_plan=[
                (self.title_scene, 9.000, 1.00),
                (self.intro_scene, 28.000, 1.00),
                (self.literature_scene, 34.000, 1.00),
                (self.nowcast_concept_scene, 11.000, 0.32),
                (self.ragged_edge_scene, 13.000, 0.42),
            ],
        )

        self.run_synced_scene_plan(
            audio_file=audio_path("Timothée Dangleterre - Script.mp4"),
            scene_plan=[
                (self.surprises_concept_scene, 24.000, 1.00),
                (self.surprises_graph_scene, 23.000, 1.00),
                (self.fragmentation_concept_scene, 23.000, 1.00),
                (self.fragmentation_graph_scene, 25.979, 1.00),
            ],
        )

        self.run_synced_scene_plan(
            audio_file=audio_path("Nathan Granier - Script 3.mp4"),
            scene_plan=[
                (self.monetary_policy_scene, 18.000, 0.62),
                (self.static_regression_results_scene, 31.000, 1.00),
                (self.irf_results_scene, 14.000, 0.55),
                (self.conclusion_scene_final3s, 21.448, 1.00),
            ],
        )