/************************** THE START OF REPORT #16 **************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#define MR				7
#define MC				9
#define BR				4
#define BC				4

#define SEL_FILE		0
#define SEL_RANDOM		1

#define CMD_BACK		-1
#define CMD_QUIT		-2

typedef int cel_type;
typedef cel_type mat_type[MR][MC];
typedef cel_type blk_type[BR][BC];

int result;

void copy_block(blk_type dst,blk_type src)
{
	int r,c;

	for(r=0;r<BR;++r)
		for(c=0;c<BC;++c)
			dst[r][c]=src[r][c];
}

int count_blk(blk_type bk,cel_type v)
{
	int r,c;
	int cnt=0;

	for(r=0;r<BR;++r)
		for(c=0;c<BC;++c)
			if(bk[r][c]==v) cnt++;
	return cnt;
}

void rotate_cw(blk_type bk)
{
	int r,c;
	cel_type temp;

	for(r=0;r<BR;++r)
		for(c=r+1;c<BC;++c) {
			temp=bk[c][r];
			bk[c][r]=bk[r][c];
			bk[r][c]=temp;
		}
	for(r=0;r<BR;++r)
		for(c=0;c<BC/2;++c) {
			temp=bk[r][BC-1-c];
			bk[r][BC-1-c]=bk[r][c];
			bk[r][c]=temp;
		}
}

void set_matrix(mat_type m,cel_type value)
{
	int r,c;

	for(r=0;r<MR;++r) 
		for(c=0;c<MC;++c)
			m[r][c]=value;
}

void copy_matrix(mat_type m_dst,mat_type m_src)
{
	int r,c;

	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c)
			m_dst[r][c]=m_src[r][c];
}

int count_mat(mat_type mt,cel_type v)
{
	int r,c;
	int cnt=0;

	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c)
			if(mt[r][c]==v) cnt++;
	return cnt;
}

int valid_mat_index(int r,int c)
{
	return 0<=r && r<MR && 0<=c && c<MC;
}

void swap_matrix(mat_type m1,mat_type m2)
{
	cel_type temp;
	int r,c;

	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c) {
			temp=m1[r][c];
			m1[r][c]=m2[r][c];
			m2[r][c]=temp;
		}
}

int equal_matrix(mat_type m1,mat_type m2)
{
	int r,c;

	for(r=0;r<MR;++r) 
		for(c=0;c<MC;++c)
			if(m1[r][c]!=0 && m2[r][c]==0 || 
				m1[r][c]==0 && m2[r][c]!=0)
				return 0;
	return 1;
}

void print_matrix(mat_type m)
{
	int r,c;

	printf("   ");
	for(c=0;c<MC;++c)
		printf("%2d",c);
	printf("\n");

	for(r=0;r<MR;++r) {
		printf("%2d: ",r);
		for(c=0;c<MC;++c) {
			if(m[r][c]!=0) 
				printf("# ");
			else 
				printf(". ");
		}
		printf("\n");
	}
}

int erase_matrix(mat_type ml[],int ml_num,int inx)
{
	swap_matrix(ml[inx],ml[--ml_num]);
	return ml_num;
}

int find_matrix(mat_type ml[],int ml_num,mat_type m)
{
	int i;

	for(i=0;i<ml_num;++i) {
		if(equal_matrix(ml[i],m)) 
			return i;
	}
	return i;
}

int erase_duplication(mat_type ml[],int ml_num)
{
	int i,ml_id;

	for(i=0;i<ml_num;++i) {
		while((ml_id=find_matrix(ml+i+1,ml_num-i-1,ml[i]))<ml_num-i-1) {
			erase_matrix(ml+i+1,ml_num-i-1,ml_id);
			--ml_num;
		}
	}
	return i;
}

void locate(mat_type ch,int r0,int c0,blk_type bk)
{
	int br,bc;

	for(br=0;br<BR;++br)
		for(bc=0;bc<BC;++bc)
			if(valid_mat_index(r0+br,c0+bc))
				ch[r0+br][c0+bc]=bk[br][bc];
}

int possible_locations(mat_type fc[],blk_type bk)
{
	mat_type ch;
	int r,c;
	int ps_cnt;
	int cnt_blk;

	ps_cnt=0;
	cnt_blk=count_blk(bk,1);
	for(r=1-BR;r<MR;++r) {
		for(c=1-BC;c<MC;++c) {
			set_matrix(ch,0);
			locate(ch,r,c,bk);
			if(count_mat(ch,1)==cnt_blk)
				copy_matrix(fc[ps_cnt++],ch);
		}
	}
	return ps_cnt;
}

int make_full_choices(mat_type fc[])
{
	static cel_type blocks[][BR][BC] = {
		{{1,0,0,0},{1,0,0,0},{1,0,0,0},{1,0,0,0}},
		{{1,1,0,0},{1,1,0,0},{0,0,0,0},{0,0,0,0}},
		{{1,1,1,0},{0,1,0,0},{0,0,0,0},{0,0,0,0}},
		{{1,0,0,0},{1,1,0,0},{0,1,0,0},{0,0,0,0}},
		{{1,1,0,0},{0,1,1,0},{0,0,0,0},{0,0,0,0}},
		{{1,0,0,0},{1,0,0,0},{1,1,0,0},{0,0,0,0}},
		{{1,1,1,0},{0,0,1,0},{0,0,0,0},{0,0,0,0}}
	};
	static const int num_blocks=sizeof(blocks)/sizeof(blocks[0]);
	blk_type bk;
	int i,rt;
	int fc_cnt;

	fc_cnt=0;
	for(i=0;i<num_blocks;++i) {
		copy_block(bk,blocks[i]);
		for(rt=0;rt<4;++rt) {
			fc_cnt+=possible_locations(fc+fc_cnt,bk);
			rotate_cw(bk);
			fc_cnt=erase_duplication(fc,fc_cnt);
		}
	}
	return fc_cnt;
}
/*************************** THE END OF REPORT #16 ***************************/

/*************************** start Source about file**************************/
void fprint_mat(FILE* fp,mat_type m)
{
	int r,c;

	for(r=0;r<MR;++r) {
		for(c=0;c<MC;++c)
			if(m[r][c]) fprintf(fp,"# ");
			else fprintf(fp,". ");
		fprintf(fp,"\n");
	}
}

int  load_mat(char* file_name,mat_type mt)
{
	FILE* fp;
	int r,c;

	fp=fopen(file_name,"rt");
	if(fp==NULL) {
		perror("Error: ");
		return -1;
	}
	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c)
			if(fscanf(fp," %d",&(mt[r][c]))!=1)
				return -2;
	fclose(fp);
	return 0;
}

int  save_mat(char* file_name,mat_type mt)
{
	FILE* fp;
	int r,c;

	fp=fopen(file_name,"wt");
	if(fp==NULL) {
		perror("Error: ");
		return -1;
	}
	for(r=0;r<MR;++r) {
		for(c=0;c<MC;++c)
			fprintf(fp," %d",mt[r][c]);
		fprintf(fp,"\n");
	}
	fclose(fp);
	return 0;
}

void rand_mat(mat_type mt,cel_type v1,cel_type v2,int v1_num)
{
	int r,c;
	int rand_r,rand_c;
	cel_type temp;
	int cnt;

	cnt=0;
	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c)
			mt[r][c]=(cnt++<v1_num?v1:v2);

	for(r=0;r<MR;++r)
		for(c=0;c<MC;++c) {
			rand_r=rand()%MR;
			rand_c=rand()%MC;
			temp=mt[r][c];
			mt[r][c]=mt[rand_r][rand_c];
			mt[rand_r][rand_c]=temp;
		}
}
int init_mat(mat_type mt) 
{
	char file_name[512];
	char buffer[512];
	int select,value;

	do {
		fprintf(stdout,"Choose a Initial Table (File: %d, Random: %d) >> "
			,SEL_FILE,SEL_RANDOM);
		fflush(stdin);
		gets(buffer);
		sscanf(buffer," %d", &select);
	} while( select!=0 && select!=1);

	switch(select) {
		case SEL_FILE:
			fprintf(stdout,"Input the file name >> ");
			fflush(stdin);
			sscanf(gets(buffer)," %s",file_name);
			fprintf(stdout,"");

			if(load_mat(file_name,mt)==0) {
				return 0;
			}
			break;
		case SEL_RANDOM:
			fprintf(stdout,"Input the number of random cells >> ");
			fflush(stdin);
			sscanf(gets(buffer)," %d",&value);
			fprintf(stdout,"");

			if(0<=value && value<=MR*MC) {
				rand_mat(mt,1,0,value);
				return 0;
			}
			break;
	}
	return -1;
}
/************************** End Source about file   **************************/
/************************** THE START OF REPORT #21 **************************/
void print_matrix_double(mat_type m1,mat_type m2)
{
	int r,c;
	printf("   ");
	for(c=0;c<MC;++c)
		printf("%2d",c);
	printf("\t\t");
	printf("   ");
	for(c=0;c<MC;++c)
		printf("%2d",c);
	printf("\n");

	for(r=0;r<MR;++r) {
		printf("%2d: ",r);
		for(c=0;c<MC;++c) {
			if(m1[r][c]!=0) 
				printf("# ");
			else 
				printf(". ");
		}
		printf("\t\t");
		printf("%2d: ",r);
		for(c=0;c<MC;++c) {
			if(m2[r][c]!=0) 
				printf("# ");
			else 
				printf(". ");
		}
		printf("\n");
	}

}
void set_matrix_num(mat_type m,cel_type val_1,cel_type val_2,int num_value1)
{
	int r,c;
	int cnt;

	cnt=0;
	for(r=0;r<MR;++r) 
		for(c=0;c<MC;++c)
			m[r][c]=(cnt++<num_value1 ? val_1 : val_2);
}

void shuffle_matrix(mat_type m)
{
	int r,c;
	int rand_r,rand_c;
	cel_type temp;

	for(r=0;r<MR;++r)
	{
		for(c=0;c<MC;++c)
		{
			rand_r=rand()%MR;
			rand_c=rand()%MC;
			temp=m[r][c];
			m[r][c]=m[rand_r][rand_c];
			m[rand_r][rand_c]=temp;
		}
	}
}

void random_matrix(mat_type m,cel_type val_1,cel_type val_2,int num_val_1)
{
	set_matrix_num(m,val_1,val_2,num_val_1);
	shuffle_matrix(m);
}

int copy_matrix_list(mat_type ml_dst[],mat_type ml_src[],int ml_num)
{
	int i;
	
	for(i=0;i<ml_num;i++)
		copy_matrix(ml_dst[i],ml_src[i]);

	return i;
}

int is_overlap(mat_type m1,mat_type m2)
{
	int r,c;

	for(r=0;r<MR;++r) 
		for(c=0;c<MC;++c)
			if(m1[r][c]!=0 && m2[r][c]!=0)
				return 1;
	return 0;
}

int erase_impossible(mat_type ml[],int ml_num,mat_type m)
{
	int i;

	for(i=0;i<ml_num;++i) 
	{
		if(is_overlap(ml[i],m)==1)
		{
			swap_matrix(ml[i],ml[ml_num-1]);
			--ml_num;
			--i;
		}
	}

	return ml_num;
}

void print_matrix_list(mat_type ml[],int ml_num)
{
	int i;

	for(i=0;i<ml_num;++i) {
		printf("matrix #%d\n",i);
		print_matrix(ml[i]);
		printf("\n");
	}
}

int valid_block(int r1,int c1,int r2,int c2,int r3,int c3,int r4,int c4)
{
	return 
		0<=r1 && r1<MR && 0<=c1 && c1<MC &&
		0<=r2 && r2<MR && 0<=c2 && c2<MC &&
		0<=r3 && r3<MR && 0<=c3 && c3<MC &&
		0<=r4 && r4<MR && 0<=c4 && c4<MC;
}

void get_matrix_from_string(char* str,mat_type t)
{
	int r1,r2,r3,r4;
	int c1,c2,c3,c4;
	
	sscanf(str," %d %d %d %d %d %d %d %d",
		&r1,&c1,&r2,&c2,&r3,&c3,&r4,&c4);
	set_matrix(t,0);
	if(valid_block(r1,c1,r2,c2,r3,c3,r4,c4)) {
		t[r1][c1]=1;
		t[r2][c2]=1;
		t[r3][c3]=1;
		t[r4][c4]=1;
	}
}

int get_block(mat_type poss_cho[],int pc_num,mat_type full_cho[],int fc_num)
{
	cel_type t[MR][MC];
	char line[1024];
	char cmd[1024];
	int pc_id,fc_id;


	while(1) { /* 무한 LOOP: 잘못된 입력일 경우 계속 반복한다. */
		printf("[Team #08] Input >> ");
		fflush(stdin);
		gets(line);
		sscanf(line," %s",cmd);
		if(strcmp(cmd,"back")==0) { /* 일반 명령어가 입력된 경우 */
			return CMD_BACK;
		}
		else if(strcmp(cmd,"quit")==0) { /* 일반 명령어가 입력된 경우 */
			return CMD_QUIT;
		}
		else if(cmd[0]=='#') { /* full choice id가 입력된 경우 */
			
			sscanf(line,"#%d",&fc_id);
			for(pc_id=0;pc_id<pc_num;pc_id++)
				if(equal_matrix(full_cho[fc_id],poss_cho[pc_id]))
		    		return pc_id;
		}
		else { 
			get_matrix_from_string(line,t);
			for(pc_id=0;pc_id<pc_num;pc_id++)
					if(equal_matrix(t,poss_cho[pc_id]))
						return pc_id;

		}
	}
	return pc_num; /* 아무 의미 없는 리턴 */
}

void copy_poss_cho(mat_type current_mat,mat_type poss_cho[],int val)
{
	int r,c;

	for (r=0; r<MR; r++)
		for (c=0; c<MC; c++)
			if (current_mat[r][c]==0)
				current_mat[r][c]=poss_cho[val][r][c];
}

int is_possible(mat_type m1,mat_type m2)
{
	int r,c;

	for (r=0; r<MR; r++)
		for (c=0; c<MC; c++)
			if(m1[r][c] || m2[r][c]) return 0;
	return 1;
}

void add(mat_type dst,mat_type src)
{
	int r,c;

	for (r=0; r<MR; r++)
		for (c=0; c<MC; c++)
			dst[r][c]+=src[r][c];
}

void subtract(mat_type dst,mat_type src)
{
	int r,c;

	for (r=0; r<MR; r++)
		for (c=0; c<MC; c++)
			dst[r][c]-=src[r][c];
}

int win(mat_type m,mat_type poss_cho[],int pc_num,time_t limit)
{
	
	int i;

	result=0;
	
	if(time(NULL)>=limit) return -1;

	for(i=0;i<pc_num;++i) {
		if(is_possible(m,poss_cho[i])) {
			add(m,poss_cho[i]);
			result=result || !win(m,poss_cho,pc_num,limit);
			subtract(m,poss_cho[i]);
		}
	}
	return result;
}

int check(int n,int c,int i)
{
	return n>=i && c!=i;
}

void player_select(mat_type current_mat,mat_type poss_cho[],int ret_val)
{
	copy_poss_cho(current_mat,poss_cho,ret_val);
}

void computer_random(mat_type current_mat,mat_type poss_cho[],int pc_num)
{	
  printf("Time Over: Unknown Choice\n"); 
	result=rand()%pc_num;
	copy_poss_cho(current_mat,poss_cho,result);
}

void computer_select(mat_type current_mat,mat_type poss_cho[],int pc_num)
{ 
 result=win(current_mat,poss_cho,pc_num,time(NULL)+10);
 if(result<0) {
  computer_random(current_mat,poss_cho,pc_num);
 }
 else {
  printf("Random Choice\n");
  copy_poss_cho(current_mat,poss_cho,result);
 }
 
}
void choice()
{
	mat_type full_cho[1024]; /* the list of full choices */
	mat_type poss_cho[1024]; /* the list of possible choices */
	mat_type current_mat;
	int fc_num;
	int pc_num;
	int ret_val;
	int fs;
	srand((unsigned)time(NULL));
	
	if(init_mat(current_mat)<0) {
		return -1;
	}
	
	printf("Input Turn (COMPUTER: 0, HUMAN: 1) >>");
	scanf ("%d" , &fs);
	printf("\n(Current Table)\n");
	print_matrix(current_mat);	
	fc_num=make_full_choices(full_cho);

	pc_num=fc_num;
	while(pc_num!=0) {
		
        pc_num=copy_matrix_list(poss_cho,full_cho,fc_num);
        pc_num=erase_impossible(poss_cho,pc_num,current_mat);
		printf("Num. of PC = %d\n\n",pc_num);	
		
		if(fs==0) goto LC;
		
LS:		ret_val=get_block(poss_cho,pc_num,full_cho,fc_num);
		
	    if(ret_val==CMD_BACK) {
			printf("\nret_val: CMD_BACK\n");
		}
		else if(ret_val==CMD_QUIT) {
			printf("\nret_val: CMD_QUIT\n");
		}
		else {			
			player_select(current_mat,poss_cho,ret_val);			
			printf ("\n(Human choice) \t\t\t(Current Table)\n");
			print_matrix_double(poss_cho[ret_val],current_mat);

			printf("FC-ID= #%d\t\t\t",
				find_matrix(full_cho,fc_num,poss_cho[ret_val])
				);
			pc_num=erase_impossible(poss_cho,pc_num,current_mat);
			printf("Num. of PC = %d \n\n",pc_num);
		}
	
LC:		if (pc_num==0) printf("Computer Lose\n");
		else
		{			
			computer_select(current_mat,poss_cho,pc_num);
			
			printf ("\n(Computer choice) \t\t(Current Table)\n");
			print_matrix_double(poss_cho[result],current_mat);
			printf("FC-ID = #%d\t\t\t",
				find_matrix(full_cho,fc_num,poss_cho[result])
				);
			pc_num=erase_impossible(poss_cho,pc_num,current_mat);			
			pc_num=copy_matrix_list(poss_cho,full_cho,fc_num);
			pc_num=erase_impossible(poss_cho,pc_num,current_mat);
			

			printf("Num. of PC = %d \n\n",pc_num);
			fs=-1;
			if (pc_num==0) printf("You Lose\n");
			else goto LS;
		}	

	}

	
}

int main(void)
{
	mat_type full_cho[1024];
	printf("***** Blank Painting Game (7X9) *****\n");
	printf("Team #08: Kim Gyu-min, Yoon Seong-yong, Baek Ju-hong\n");
	printf("The Limit of Time = 10 second(s) \n");
	printf("Full Choices = %d\n\n",make_full_choices(full_cho));
	choice();


	return 0;
}



/*************************** THE END OF REPORT #21 ***************************/