這裡所有的做法跟步驟，專案方向都是遵守`@big_data.md`，資料處理跟執行遵守`@app_formula_design.md` ，並且==`@app_formula_design.md` 是遵守`@big_data.md`==

| 原始                     | 命名          |
| ---------------------- | ----------- |
| iMarine航港發展資料庫-臺灣進口貨空櫃 | enter-empty |
| iMarine航港發展資料庫-臺灣出口貨空櫃 | exit-empty  |
| iMarine航港發展資料庫-臺灣出口貨實櫃 | exit-full   |
| iMarine航港發展資料庫-臺灣進口貨實櫃 | enter-full  |
| 臺灣地區國際商港貨櫃裝卸量          | throughput  |
檔案說明：
- `build_master_table.R`  
	- 用途是：「生出主表」
	- 做法：
		- 第一步：先建立 port_map，只放你確定的主港代碼
		- 第二步：把四張流量表（除了throughput)彙總成 empty_in / empty_out / full_in / full_out 四個主要變數 （後面的其他變數都是這四個做變形）
		- 第三步：把 throughput.csv 篩出 2025 （因為四大表只有2025這年，資料時間軸要對齊）且同一批港口（**所以我們會用ym時間 x port港口做主鍵**））
		- 第四步：合成並且ouput成 master表格（主表），後續要用
	- *到這部已完成 @app_formula_design.md 的 2.2全部＋2.3的前兩列）*
- `check_master_table.R` 
	- 用途是：「讀前面生成的`build_master_table.R` （稱他主表key table）驗證它」
	- 要驗證：
		- 必要欄位有沒有都存在
		- ym + port 是否重複（ **ym + port是我們的資料表主鍵，是複合主鍵**）
		- 哪些欄位有 NA值
		- 哪些列的 throughput 是 0 或缺值
		- 有沒有不合理的負值
		- 每個港口有幾個月份
		- 每個港口的全年加總摘要
	- 執行驗證後的結果是我們要回頭去主表做資料清洗的依據（如圖）
		- ![[BigData_check_master_table_output1.png|602]]
			- ym + port 主鍵沒重複，表示這個主鍵是可以辨識唯一值的
			- 必要欄位都有
			- **有 23 筆 throughput = 0 或缺值**
		- ![[BigData_check_master_table_output2.png]]
			- 這 23 筆主要來自：
			    - 花蓮港 12 個月都 throughput = 0
			    - 蘇澳港 幾乎整年 throughput = 0，全年總吞吐只有 4
			    - **這部分導致了 empty_share、net_pressure_raw、export_pull 才會出現 NA**
		- ![[BigData_check_master_table_output3.png]]
			- 反應：每個港口在主表裡有幾個月份
			- 結果：主表目前對每個港口都成功建立了 2025/01 到 2025/12 共 12 個月份，表是前面的build_master(主表)的結構是完整的，主表結構沒有缺月問題
		- ![[BigData_check_master_table_output4.png]]
			- 反應：每個港口全年加總後的量級與合理性
			- 欄位含義：參`@app_formula_design.md`
			- 結果：
				- 高雄港 / 基隆港 / 臺中港 / 臺北港 都有明確作業量，屬於可分析核心港口
				- 安平港 雖然量很小，但仍不是 0，就先保留
				- 蘇澳港 幾乎沒有量，total_throughput = 4
				- 花蓮港 完全是 0
			- **有資料列，但不代表有分析意義，但可以作為後續分析樣本應該保留哪些港口的依據**，是後續建立analysis_table的規則來源
	- 這步驟完成就往下建立analysis_table，依循check表的結果
- `build_analysis_table.R`
	- 會產出 analysis_table_2025.csv 
	- 用途是從主表篩出可分析樣本
	- 有加入部分規則例如踢除throughpu <= 0的港口