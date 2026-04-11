-- === Plugins ===
vim.pack.add({
  "https://github.com/nvim-lualine/lualine.nvim",
})

-- === General Settings ===
vim.opt.number = true
vim.opt.numberwidth = 5
vim.opt.showcmd = true
vim.opt.cursorline = true
vim.opt.wildmenu = true
vim.opt.lazyredraw = true

-- === Indentation ===
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smartindent = true

-- === Search ===
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- === File handling ===
vim.opt.hidden = true
vim.opt.undofile = true
vim.opt.swapfile = false

-- === Appearance ===
vim.opt.background = "dark"
vim.opt.termguicolors = true
vim.opt.laststatus = 2

vim.api.nvim_set_hl(0, "LineNr", { bold = true, fg = "DarkGray" })
vim.api.nvim_set_hl(0, "CursorLineNr", { bold = true, fg = "DarkGray" })
vim.api.nvim_set_hl(0, "CursorLine", { bold = true })

-- === Plugin configuration ===
require("lualine").setup({
  options = {
    theme = "auto",
  },
})
