interface FilterBarProps {
  filters: { teams: string[], years: number[], positions: string[] }
  selectedPos: string
  onPosChange: (val: string) => void
}

export default function FilterBar({ filters, selectedPos, onPosChange }: FilterBarProps) {
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
      {/* Team and Year follow the same pattern */}
    </div>
  )
}