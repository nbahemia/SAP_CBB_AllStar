import type { Filters } from "../types"

interface FilterBarProps {
  filters: Filters
  selectedPos: string
  selectedTeam: string
  selectedYear: string
  onPosChange: (val: string) => void
  onTeamChange: (val: string) => void
  onYearChange: (val: string) => void
}

export default function FilterBar({
  filters,
  selectedPos,
  selectedTeam,
  selectedYear,
  onPosChange,
  onTeamChange,
  onYearChange,
}: FilterBarProps) {
  return (
    <div className="flex flex-col gap-4">
      {/* Position */}
      <div className="flex flex-col gap-1.5">
        <label className="text-xs text-[#7a8fa6] tracking-wider">POSITION</label>
        <select
          className="bg-[#0f1e30] border border-[#1e3a5f] text-[#e8f0fe] px-3 py-2.5 rounded-lg text-sm w-full"
          value={selectedPos}
          onChange={e => onPosChange(e.target.value)}
        >
          <option value="">All Positions</option>
          {filters.positions.map(p => (
            <option key={p} value={p}>{p === "G" ? "Guard" : p === "F" ? "Forward" : "Center"}</option>
          ))}
        </select>
      </div>

      {/* Team */}
      <div className="flex flex-col gap-1.5">
        <label className="text-xs text-[#7a8fa6] tracking-wider">TEAM</label>
        <select
          className="bg-[#0f1e30] border border-[#1e3a5f] text-[#e8f0fe] px-3 py-2.5 rounded-lg text-sm w-full"
          value={selectedTeam}
          onChange={e => onTeamChange(e.target.value)}
        >
          <option value="">All Teams</option>
          {filters.teams.map(t => (
            <option key={t} value={t}>{t}</option>
          ))}
        </select>
      </div>

      {/* Year */}
      <div className="flex flex-col gap-1.5">
        <label className="text-xs text-[#7a8fa6] tracking-wider">YEAR</label>
        <select
          className="bg-[#0f1e30] border border-[#1e3a5f] text-[#e8f0fe] px-3 py-2.5 rounded-lg text-sm w-full"
          value={selectedYear}
          onChange={e => onYearChange(e.target.value)}
        >
          <option value="">All Years</option>
          {filters.years.map(y => (
            <option key={y} value={String(y)}>{y}</option>
          ))}
        </select>
      </div>
    </div>
  )
}