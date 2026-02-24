import type { PredictResult } from "../types"
import ProbabilityGauge from "./ProbabilityGauge"

interface PlayerCardProps {
  result: PredictResult
}

const POSITION_LABELS: Record<string, string> = {
  G: "Guard",
  F: "Forward",
  C: "Center",
}

export default function PlayerCard({ result }: PlayerCardProps) {
  return (
    <div className="bg-[#0f1e30] border border-[#1e3a5f] rounded-2xl p-4 flex justify-between items-center">
      <div>
        <h2 className="text-xl font-black tracking-widest leading-none uppercase">
          {result.player_name}
        </h2>
        <p className="text-xs text-[#7a8fa6] mt-1 tracking-wide">
          {result.team} · {POSITION_LABELS[result.position] ?? result.position} · {result.year}
        </p>
      </div>
      <ProbabilityGauge value={result.all_star_probability} />
    </div>
  )
}