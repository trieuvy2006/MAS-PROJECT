# MAS291 R Project - Hướng dẫn cho người mới

Project này chứa toàn bộ Task 1-5 bằng R:

- `analysis.R`: code tính toán, kiểm định và tạo biểu đồ.
- `MAS291_Report.Rmd`: báo cáo hoàn chỉnh.
- `setup_packages.R`: cài gói cần thiết để xuất báo cáo.
- `MAS291_R_Project.Rproj`: mở project bằng RStudio.
- `results/`: các bảng kết quả và toàn bộ dataset có cột STT được tạo sau khi chạy.
- `figures/`: các biểu đồ được tạo sau khi chạy.

## Bước 1 - Cài R và RStudio

1. Cài R từ https://cran.r-project.org/
2. Cài RStudio Desktop từ https://posit.co/download/rstudio-desktop/

## Bước 2 - Mở project

Mở file `MAS291_R_Project.Rproj` bằng RStudio.

## Bước 3 - Chạy phân tích

Mở `analysis.R`, sau đó bấm nút `Source` ở góc trên bên phải cửa sổ code.

Khi hoàn thành, Console sẽ hiện:

```text
MAS291 R analysis completed successfully.
```

Các bảng sẽ nằm trong `results`, còn biểu đồ nằm trong `figures`.

## Bước 4 - Xuất báo cáo HTML

Chạy `setup_packages.R` một lần để cài `rmarkdown` và `knitr`.

Sau đó mở `MAS291_Report.Rmd` và bấm `Knit`. RStudio sẽ tạo file `MAS291_Report.html` có nội dung Task 1-5, bảng, biểu đồ và kết luận.

Để tạo PDF nộp cô: mở `MAS291_Report.html` bằng Chrome/Edge, bấm `Ctrl + P`, chọn `Save as PDF`.

## Nếu R báo không tìm thấy dữ liệu

Đảm bảo file dữ liệu đang tồn tại tại:

```text
D:/prjcomputer/online+shoppers+purchasing+intention+dataset/online_shoppers_intention.csv
```

Hoặc sao chép `online_shoppers_intention.csv` vào cùng thư mục với `analysis.R`.

## Chia phần thuyết trình

1. Người 1: Task 1 - EDA.
2. Người 2: Task 2 - One Population.
3. Người 3: Task 3 - Two Populations.
4. Người 4: Task 4 - Regression.
5. Người 5: Introduction, Task 5, limitations và Q&A.

Không cần học thuộc code. Khi thuyết trình, tập trung vào câu hỏi nghiên cứu, biểu đồ, p-value, confidence interval và ý nghĩa thực tế.
