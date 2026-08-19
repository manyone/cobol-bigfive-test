# 🧠 Big Five Personality Test in COBOL

> *"Know thyself—preferably in 80 columns or fewer."*

A fully functional **Big Five (OCEAN) personality assessment**, implemented in **GnuCobol** and adapted to **ANSI COBOL 74**, designed to run cleanly on **TK4-** (IBM Mainframe MVS emulator).

This isn’t just a toy—it’s a faithful adaptation of a validated psychological model, written in a language that pre-dates the modern web… but still gets the job done.

---

## 📖 Table of Contents

<!-- TOC -->

- [🧠 Big Five Personality Test in COBOL](#-big-five-personality-test-in-cobol)
  - [📖 Table of Contents](#-table-of-contents)
  - [✨ Overview](#-overview)
  - [🖥️ Web Version](#web-version)
  - [📦 Files Included](#-files-included)
  - [⚙️ How It Works](#️-how-it-works)
    - [Scoring Logic](#scoring-logic)
    - [Interpretation Tiers](#interpretation-tiers)
    - [Sample Session](#sample-session)
    - [Sample Interpretation](#sample-interpretation)
  - [🚀 Running in TK4-!](#-running-in-tk4-)
  - [🧪 Testing & Development](#-testing--development)
  - [💡 Why COBOL?](#-why-cobol)
  - [📚 References & Methodology](#-references--methodology) 
  - [📜 License](#-license)

<!-- /TOC -->

---

## ✨ Overview

This program administers a short-form **Big Five personality inventory** (20 or 50 items), scores your responses, and provides a personalized narrative interpretation for each of the five core traits:

1. **Extraversion**  
2. **Agreeableness**  
3. **Conscientiousness**  
4. **Emotional Stability** *(aka low Neuroticism)*  
5. **Intellect** *(aka Openness to Experience)*

Originally built with 50 questions, the **20-item version** (`big5sml.*`) was added after testing revealed… well, that 50 questions is *a lot* when you’re typing responses one-by-one in MVS READY mode!

> Fun fact: Both versions use the **same scoring engine** and **same interpretation file**—just different question sets.

---

## Web Version

For a modern interface comparison, I created a [web-based version](https://manyone.github.io/cobol-bigfive-test/webapp/) using HTML/CSS/JavaScript. It's interesting to see how user interaction has evolved from terminal-based applications to graphical web interfaces, while maintaining the same core assessment logic.

**COBOL Version**: Terminal-based, character UI, runs on mainframes  
**Web Version**: Graphical, point-and-click, runs in browsers

Both provide interactive experiences, but showcase different eras of human-computer interaction.

---

## 📦 Files Included

| File           | Purpose                                                                                     |
| -------------- | ------------------------------------------------------------------------------------------- |
| `bigfive.cob`  | Main COBOL program in GnuCobol                                                              |
| `tkbig5.cob`   | Main COBOL program (adapted from `bigfive.cob` for TK4- )                                   |
| `big5sml.dat`  | 20-item short-form statements                                                               |
| `big5sml.txt`  | for easy edit (convert to `big5sml.dat`  using `txt2dat.ps1` below)                         |
| `big5med.dat`  | 50-item medium-form statements                                                              |
| `big5med.txt`  | for easy edit (convert to `big5med.dat`  using `txt2dat.ps1` below)                         |
| `big5defs.dat` | Trait definitions + narrative interpretations (low/average/high)                            |
| `tkclist.txt`  | CLIST script to run the test in TK4-                                                        |
| `txt2dat.ps1`  | powershell script converts `.txt` to `.dat` (see `Testing and Development` below for usage) |
| `README.md`    | You’re reading it!                                                                          |

All `.dat` files use a **self-describing format** (see below).

---

## ⚙️ How It Works

### Scoring Logic

Each line in the statements file (`big5sml.dat` / `big5med.dat`) follows this convention:

```
3  I get chores done right away.
1- I don't talk a lot.
```

- **First character**: Trait ID (`1`–`5`)
- **Second character (optional)**: `-` means the item is **reverse-scored**
  - If user enters `R` (1–5), actual score = `6 - R`

Responses are summed per trait, then converted to a percentage:

```cobol
COMPUTE PCT-TRAIT = (SUM-TRAIT / MAX-POSSIBLE) * 100
```

### Interpretation Tiers

Based on the percentage score:

- **< 34%** → **Low** (`1`)
- **34–66%** → **Average** (`2`)
- **> 66%** → **High** (`3`)

The program then looks up the corresponding narrative in `big5defs.dat`:

```
10Extraversion measures your sociability...
11You prefer solitude...                 ← Low (1)
12You engage socially with moderate...   ← Average (2)
13You thrive in social environments...   ← High (3)
```

Final output includes:

- Trait definition
- Personalized interpretation  
- All word-wrapped to **≤80 characters** for terminal readability

### Sample Session

```prompts
BIG FIVE PERSONALITY TEST         (20 items)
Please respond to all of the statements and answer in sequence.
For each statement choose the response that best represents your opinion:

 1. I am the life of the party.
1=Very Inaccurate  2=Inaccurate  3=Neutral  4=Accurate  5=Very Accurate
Select the answer that best applies to you
2

 2. I sympathize with others' feelings.
1=Very Inaccurate  2=Inaccurate  3=Neutral  4=Accurate  5=Very Accurate
Select the answer that best applies to you
```

### Sample Interpretation

```
EXTRAVERSION          45.0%
Extraversion measures your sociability, assertiveness, and tendency to seek
stimulation in social settings.
You engage socially with moderate enthusiasm, enjoying group activities while
valuing personal downtime.

AGGREABLENESS         50.0%
Agreeableness measures your tendency to be compassionate and cooperative rather
than suspicious and antagonistic toward others.
You balance cooperation with healthy boundaries, being generally trusting while
asserting your needs appropriately.

CONSCIENTIOUSNESS     60.0%
Conscientiousness measures your tendency to be organized, responsible, and
self-disciplined.
You demonstrate reliable goal-directed behavior with flexible tendencies,
balancing structure with adaptability.

EMOTIONAL STABILITY   75.0%
Emotional Stability measures your capacity to remain calm, composed, and
resilient in the face of stress and adversity.
You remain calm, composed, and resilient under pressure, maintaining emotional
equilibrium even during challenging situations.

INTELLECT             85.0%
Openness measures your appreciation for art, emotion, adventure, imagination,
curiosity, and variety of experiences.
You embrace unconventional ideas, creative pursuits, and intellectual
exploration with enthusiasm.
```

---

## 🚀 Running in PC (GnuCobol)

### Step 1: Compile the Program

```bash
cobc -c bigfive.cob
```

### Step 2: Copy desired file to designated input

```bash
copy big5sml.* big5test.*
```

### Step 3: Go!

```bash
bigfive
```

## 🚀 Running in TK4-

### 1. Transfer Files to TK4

Use `IND$FILE` to copy the **`.txt` source files** from your PC to MVS.  
**Do not send `.dat` files**—instead, transmit the `.txt` versions and let `IND$FILE` format them as fixed-length records during transfer.

| Source (PC)    | Target (MVS)                     | Record Length | Notes                    |
| -------------- | -------------------------------- | ------------- | ------------------------ |
| `tkbig5.cob`   | `'USERID.SOURCE.COBOL(BIGFIVE)'` | —             | Source code (member)     |
| `tkclist.txt`  | `'USERID.RUN.CLIST(BIGFIVE)'`    | —             | CLIST (member)           |
| `big5sml.txt`  | `'USERID.BF.SML.DAT'`            | **80**        | Statements (short form)  |
| `big5med.txt`  | `'USERID.BF.MED.DAT'`            | **80**        | Statements (medium form) |
| `big5defs.txt` | `'USERID.BF.TRAITS.DAT'`         | **150**       | Trait definitions        |

> ⚠️ **Critical**: In `IND$FILE`, set **File Type = FIXED** and specify the exact **Record Length** shown above.  
> The `.txt` files on your PC should be plain text (no extra padding). `IND$FILE` will convert them to fixed-block MVS datasets automatically.

2. **Compile**: use standard cobol74 compile procedure

3. **Run** via CLIST (specify parameter `'SML'` or `'MED'` according to desired file of statements):
   
   ```tso
   EX RUN(BIGFIVE) 'SML'
   ```
   
   (Note: Below is the `BIGFIVE` clist for reference).
   
   ```tso
   PROC 1 SIZE    
   ALLOC FILE(STMTS) DA('USERID.BF.&SIZE..DAT') SHR  
   ALLOC FILE(TRAITS) DA('USERID.BF.TRAITS.DAT') SHR 
   ALLOC FILE(SYSOUT) DA(*)     
   ALLOC FILE(SYSIN) DA(*)    
   CALL 'USERID.RUN.LOAD(BIGFIVE)'  
   ```
   
    For example, `EX RUN(BIGFIVE) 'SML'` causes `&SIZE` to resolve to `SML`, so `STMTS` is allocated to `'USERID.BF.SML.DAT'`

> Tested and working in **TK4-** under MVS 3.8j.

---

## 🧪 Testing & Development

- **Auto-response mode**: Built-in randomized input for fast testing (needs recompiling after enabling the randomizing code)
- **Dual datasets**: copy desired version to the expected input file.
  `copy big5sml.* big5test.*`  or
  `copy big5med.* big5test.*` 
- **COBOL 74 compliant**: Maximizes portability across legacy and hobbyist systems
- **txt2dat.ps1**: this is a powershell script intended to run in the pc to convert a .txt file to .dat format with specified length (eg. fixed blocked files used in GnuCOBOL).    
  `.\txt2dat big5sml.txt 80` would create `big5sml.dat` with a fixed length of 80 characters.

---

## 💡 Why COBOL?

Because sometimes the best way to understand yourself…  
is through a language that values **clarity**, **structure**, and **reliability**—even if it’s older than your therapist.

*(Also: it runs on a mainframe. That’s just cool.)*

---

## 📚 References & Methodology

This project is built upon established psychometric frameworks and open-source resources:

- **Trait Framework & Definitions**: Based on the standard Five-Factor Model (OCEAN), utilizing foundational trait definitions from the [International Personality Item Pool (IPIP)](https://ipip.ori.org/).
- **50-Item Scale & Scoring Logic**: Adapted from the [IPIP 50-Item Scale](https://ipip.ori.org/new_ipip-50-item-scale.htm), which provided the core scoring methodology and reverse-scoring conventions.
- **20-Item Short Form**: Sourced and adapted from this [Testable 20-item experiment](https://www.testable.org/experiment/661/836366/start) to provide a faster, more terminal-friendly assessment.
- **Scoring Heuristic**: Uses a standard **tertile split** (< 34% Low, 34–66% Average, > 66% High) for narrative interpretation. This is the accepted practice for standalone applications that do not have access to a massive normative population database.
- **Narrative Interpretations**: The low/average/high descriptions were condensed and re-worded from longer academic descriptions (with the assistance of AI) to ensure clarity, professional tone, and strict adherence to 80-column terminal readability.

## 📜 License

This project is open for educational and personal use.  
Feel free to adapt, share, or deploy it on your own MVS system.

—  
*Developed with care (and a lot of `DISPLAY` statements) by [manyone](https://github.com/manyone)*
