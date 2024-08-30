return {
	{
		'loganswartz/selenized.nvim',
		dependencies = {
			'rktjmp/lush.nvim',
		},
		config = function()
			vim.g.selenized_variant = 'normal'
			vim.o.background = 'light'
			vim.cmd([[colorscheme selenized]])
		end,
	},
}
