import type { PredictResult } from "../types"
import { RAW_LABELS } from "../constants"

interface StatPillsProps {
  result: PredictResult
}

export default function StatPills({ result }: StatPillsProps) {
  return (
    <div className="bg-[#0f1e30] border border-[#1e3a5f] rounded-2xl p-6">
      <span className="text-xs text-[#7a8fa6] tracking-[3px] uppercase">Key Stats</span>

      <div className="grid grid-cols-[repeat(auto-fill,minmax(140px,1fr))] gap-2.5 mt-4">
        {Object.entries(RAW_LABELS).map(([key, label]) => (
          <div
            key={key}
            className="bg-[#080f1a] border border-[#1e3a5f] rounded-lg px-3 py-2"
          >
            <p className="text-[0.7rem] text-[#7a8fa6] mb-0.5">{label}</p>
            <p className="text-sm font-bold">
              {result.raw_stats?.[key] != null
                ? Math.round(result.raw_stats[key] * 100) / 100
                : "—"}
            </p>
          </div>
        ))}
      </div>
    </div>
  )
}