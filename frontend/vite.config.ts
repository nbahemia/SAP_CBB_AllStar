import { defineConfig } from "vite"
import react from "@vitejs/plugin-react"
import tailwindcss from "@tailwindcss/vite"
import { resolve } from "path"

export default defineConfig({
  plugins: [react(), tailwindcss()],
  resolve: {
    alias: {
      react: resolve("./node_modules/react"),
      "react-dom": resolve("./node_modules/react-dom"),
    },
  },
})