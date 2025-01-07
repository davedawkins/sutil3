import { defineConfig } from 'vite';

export default defineConfig({
    server: {
        watch: {
            ignored: [
                "**/*.fs"
            ]
        }
    },
    clearScreen: false
})