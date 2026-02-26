# CBB All-Star Predictor

A full-stack machine learning web application that predicts the probability of a college basketball player becoming an NBA All-Star based on their college statistics.

---

## Demo

<img width="3366" height="1702" alt="image" src="https://github.com/user-attachments/assets/01e0f489-1243-4fc4-8fb3-9dd195c41c25" />
> Select a player by position, team, and year — the model returns an All-Star probability score along with a radar chart of their statistical profile.

---

## How It Works

Three position-specific logistic regression models (Guard, Forward, Center) are trained on college basketball data from **2009–2021**. Each model uses 18 statistical features to predict whether a player will eventually become an NBA All-Star.

Players labeled as All-Stars include names like Anthony Davis, Ja Morant, Zion Williamson, and 46 others who were matched by name and team to their college season data.

### Features Used
| Feature | Description |
|---|---|
| `Ortg` | Offensive Rating |
| `usg` | Usage % |
| `eFG` | Effective Field Goal % |
| `TS_per` | True Shooting % |
| `ORB_per` / `DRB_per` | Offensive / Defensive Rebound % |
| `AST_per` | Assist % |
| `TO_per` | Turnover % (inverted — lower is better) |
| `blk_per` / `stl_per` | Block / Steal % |
| `porpag` | Points Over Replacement Per Adjusted Game |
| `adjoe` | Adjusted Offensive Efficiency |
| `drtg` / `adrtg` | Defensive Rating / Adjusted (inverted) |
| `dporpag` | Defensive PORPAG |
| `bpm` | Box Plus/Minus |
| `ast_tov` | Assist-to-Turnover Ratio |
| `height_in` | Height in inches |

### Model Details
- **Algorithm:** L2-regularized Logistic Regression with `class_weight="balanced"` to handle class imbalance (~49 All-Stars vs ~5,000+ non-All-Stars)
- **Tuning:** 5-fold cross-validated grid search over regularization parameter `C`
- **Performance:** AUC scores of 0.96–0.98 across positions

---

## Project Structure

```
SAP_CBB_AllStar/
├── backend/
│   ├── main.py           # FastAPI app — prediction & filter endpoints
│   ├── train_model.py          # Model training script
│   ├── utils.py          # Model loader with in-memory caching
│   ├── database.py       # SQLite DB init and query functions
│   ├── data/
│   │   └── CBB_labeled.csv
│   └── models/
│       ├── g_model.pkl
│       ├── f_model.pkl
│       └── c_model.pkl
└── frontend/
    ├── src/
    │   ├── App.tsx
    │   ├── api.ts
    │   ├── types.ts
    │   ├── constants.ts
    │   └── components/
    │       ├── FilterBar.tsx
    │       ├── PlayerSelect.tsx
    │       ├── PlayerCard.tsx
    │       ├── ProbabilityGauge.tsx
    │       └── StatsRadar.tsx
    └── package.json
```

---

## Setting Up Locally

### Prerequisites
- Python 3.11+
- Node.js 18+
- npm

### 1. Clone the repo
```bash
git clone https://github.com/your-username/SAP_CBB_AllStar.git
cd SAP_CBB_AllStar
```

### 2. Install backend dependencies
```bash
cd backend
pip install fastapi uvicorn pandas scikit-learn joblib pydantic
```

### 3. Train the models
```bash
python train.py
```

### 4. Install frontend dependencies
```bash
cd ../frontend
npm install
```

### 5. Run the app
```bash
npm run start
```

This starts both the backend (http://localhost:8000) and frontend (http://localhost:5173) concurrently.

---

## API Endpoints

| Method | Endpoint | Description |
|---|---|---|
| `GET` | `/filters` | Returns distinct positions, teams, and years for dropdowns |
| `GET` | `/players` | Returns players filtered by position, team, and/or year |
| `POST` | `/predict` | Returns All-Star probability + stats for a given `pid` and `year` |
| `GET` | `/model-info/{position}` | Returns top 5 most important features for a position's model |

---

## Frontend Features

- Filter players by **position**, **team**, and **year**
- **Probability gauge** — color-coded green/yellow/red based on score
- **Radar chart** with percentile and raw stat modes
- **Stats grid** with per-position importance highlighting (★ = top model feature)
- Percentile inversions for lower-is-better stats (turnovers, defensive rating)

---

## Known Limitations

- Training data only covers **2009–2021** — predictions for post-2021 players are extrapolations
- All-Star labels are assigned based on eventual NBA career, not college performance directly
- Percentiles are calculated **across all positions** in the source data
- Small sample of positive labels (~49 All-Stars) limits model precision

---

## Data Source

College basketball statistics sourced from [Kaggle](https://barttorvik.com](https://www.kaggle.com/datasets/adityak2003/college-basketball-players-20092021 ), cleaned and labeled with NBA All-Star selections.

---

## Tech Stack

**Backend:** Python · FastAPI · scikit-learn · SQLite · pandas  
**Frontend:** React · TypeScript · Vite · Tailwind CSS · Recharts
