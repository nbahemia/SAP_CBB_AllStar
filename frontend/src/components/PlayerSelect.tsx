import type { Player } from "../types"

interface PlayerSelectProps {
  players: Player[]
  selectedPid: string
  selectedYear: string
  loading: boolean
  onPlayerChange: (val: string) => void
  onPredict: () => void
}

export default function PlayerSelect({
  players,
  selectedPid,
  selectedYear,
  loading,
  onPlayerChange,
  onPredict,
}: PlayerSelectProps) {
  if (players.length === 0) return null

  return (
    <div className="flex flex-col gap-4">
      {/* Player dropdown */}
      <div className="flex flex-col gap-1.5">
        <label className="text-xs text-[#7a8fa6] tracking-wider">
          PLAYER <span className="text-[#2d7dd2]">({players.length} found)</span>
        </label>
        <select
          className="bg-[#0f1e30] border border-[#1e3a5f] text-[#e8f0fe] px-3 py-2.5 rounded-lg text-sm w-full cursor-pointer focus:outline-none focus:border-[#2d7dd2]"
          value={selectedPid}
          onChange={(e) => onPlayerChange(e.target.value)}
        >
          <option value="">Select a player...</option>
          {players.map((p) => (
            <option key={`${p.pid}-${p.year}`} value={p.pid}>
              {p.player_name} · {p.team}
            </option>
          ))}
        </select>
      </div>

      {/* Predict button */}
      <button
        onClick={onPredict}
        disabled={!selectedPid || !selectedYear || loading}
        className="w-full py-3.5 px-8 rounded-xl text-sm font-bold tracking-wide bg-gradient-to-r from-[#2d7dd2] to-[#1a5fa8] text-white transition-all hover:opacity-90 hover:-translate-y-0.5 disabled:opacity-40 disabled:cursor-not-allowed disabled:hover:translate-y-0"
      >
        {loading ? "Running Model..." : "Predict All-Star Probability"}
      </button>
    </div>
  )
}