import { useState, useEffect } from "react"
import { getFilters, getPlayers, predict } from "./api"
import type { Filters, Player, PredictResult } from "./types"
import FilterBar from "./components/FilterBar"
import PlayerSelect from "./components/PlayerSelect"
import PlayerCard from "./components/PlayerCard"
import StatsRadar from "./components/StatsRadar"


export default function App() {
  const [filters, setFilters] = useState<Filters>({ teams: [], years: [], positions: [] })
  const [selectedPos, setSelectedPos] = useState("")
  const [selectedTeam, setSelectedTeam] = useState("")
  const [selectedYear, setSelectedYear] = useState("")
  const [players, setPlayers] = useState<Player[]>([])
  const [selectedPid, setSelectedPid] = useState("")
  const [result, setResult] = useState<PredictResult | null>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState("")

  // Load filter options on mount
  useEffect(() => {
    getFilters()
      .then(setFilters)
      .catch(() => setError("Could not connect to API. Is the backend running?"))
  }, [])

  // Reload players when filters change
  useEffect(() => {
    if (!selectedPos && !selectedTeam && !selectedYear) {
      setPlayers([])
      setSelectedPid("")
      return
    }
    getPlayers({ position: selectedPos, team: selectedTeam, year: selectedYear })
      .then((data) => {
        setPlayers(data)
        setSelectedPid("")
        setResult(null)
      })
      .catch(() => setError("Failed to load players"))
  }, [selectedPos, selectedTeam, selectedYear])

  async function handlePredict() {
    if (!selectedPid || !selectedYear) return
    setLoading(true)
    setError("")
    try {
      const data = await predict(Number(selectedPid), Number(selectedYear))
      setResult(data)
    } catch (e) {
      setError((e as Error).message)
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="min-h-screen bg-[#080f1a] text-[#e8f0fe]">

      {/* Header */}
      <header className="border-b border-[#1e2a3a] px-10 py-5 flex items-center gap-4">
        <div className="w-9 h-9 rounded-lg bg-gradient-to-br from-[#2d7dd2] to-[#00e5a0] flex items-center justify-center text-xl">
          🏀
        </div>
        <div>
          <h1 className="text-2xl font-black tracking-widest leading-none">
            CBB ALL-STAR PREDICTOR
          </h1>
          <p className="text-[0.7rem] text-[#7a8fa6] tracking-widest">
            COLLEGE BASKETBALL · ML MODEL
          </p>
        </div>
      </header>

      {/* Main layout */}
      <main className="max-w-[1100px] mx-auto px-6 py-10 grid grid-cols-[320px_1fr] gap-8 items-start">

        {/* Left panel */}
        <div className="flex flex-col gap-5">
          <span className="text-[0.7rem] text-[#7a8fa6] tracking-[3px] uppercase">
            Find a Player
          </span>

          <FilterBar
            filters={filters}
            selectedPos={selectedPos}
            selectedTeam={selectedTeam}
            selectedYear={selectedYear}
            onPosChange={setSelectedPos}
            onTeamChange={setSelectedTeam}
            onYearChange={setSelectedYear}
          />

          <PlayerSelect
            players={players}
            selectedPid={selectedPid}
            selectedYear={selectedYear}
            loading={loading}
            onPlayerChange={setSelectedPid}
            onPredict={handlePredict}
          />

          {!selectedPos && !selectedTeam && !selectedYear && (
            <p className="text-xs text-[#2d4a6a] text-center">
              Select at least one filter to find players
            </p>
          )}

          {(selectedPos || selectedTeam || selectedYear) && players.length === 0 && (
            <p className="text-xs text-[#7a8fa6] text-center">
              No players found for these filters
            </p>
          )}

          {error && (
            <div className="bg-[#2a1020] border border-[#ff4d6d] rounded-lg p-3 text-sm text-[#ff4d6d]">
              {error}
            </div>
          )}
        </div>

        {/* Right panel */}
        <div>
          {!result ? (
            <div className="border border-dashed border-[#1e3a5f] rounded-2xl py-20 text-center text-[#2d4a6a]">
              <div className="text-5xl mb-4">🏀</div>
              <p className="text-sm tracking-widest uppercase">
                Select a player and run the model
              </p>
            </div>
          ) : (
            <div className="flex flex-col gap-6">
              <PlayerCard result={result} />
              <StatsRadar result={result} />

            </div>
          )}
        </div>

      </main>
    </div>
  )
}