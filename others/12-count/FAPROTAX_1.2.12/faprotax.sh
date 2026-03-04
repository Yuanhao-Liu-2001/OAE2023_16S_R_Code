#教程来自于https://blog.csdn.net/woodcorpse/article/details/106552599
#cd /data/home/yuanhao/data/Sanmen_Island/test_16S/faprotax-sanmen/FAPROTAX_1.2.6
python collapse_table.py \
	-i /home/yuanhao/Mesocosms/2023SummerWinter/02-PROKs/final_no_cp/feature-table_10142.tsv \
	-g /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/FAPROTAX.txt \
	-o /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/result/feature-table_10142.faprotax \
	-s /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/result/result_sub \
	-r /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/result/feature-table_10142.report \
	--out_groups2records_table /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/result/feature-table_10142.groups2records_table \
	--out_group_overlaps /home/yuanhao/Mesocosms/2023SummerWinter/FAPROTAX_1.2.12/result/feature-table_10142.group_overlaps \
	--column_names_are_in last_comment_line \
	-d 'taxonomy' \
	-c '#' \
	-v \
	--force 