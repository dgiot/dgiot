import os
from openai import OpenAI
from glob import glob
from pymilvus import model as milvus_model
from pymilvus import MilvusClient
from transformers import AutoModel, AutoTokenizer
from huggingface_hub import hf_hub_download
from tqdm import tqdm
import json
import sys
import base64

# pip install --upgrade pymilvus[model] openai requests tqdm

# 1、 准备数据
def create_device(params):

    productid = params['productid']
    deviceid = params['deviceid']
    path = params['path']
    print(path)

    text_lines = []
    for file_path in glob(path, recursive=True):
        with open(file_path, "r") as file:
            file_text = file.read()
        text_lines += file_text.split("# ")
        # print(text_lines)

    os.environ["HF_ENDPOINT"] = "https://hf-mirror.com"
    embedding_model = milvus_model.DefaultEmbeddingFunction()

    # 生成测试向量，并输出向量维度以及测试向量的前几个元素。
    test_embedding = embedding_model.encode_queries(["This is a test"])[0]
    embedding_dim = len(test_embedding)
    print(embedding_dim)
    print(test_embedding[:10])

    #将数据加载到Milvus
    #对于MilvusClient需要说明：将uri设置为本地文件，例如./milvus. db，是最方便的方法，因为它会自动使用Milvus Lite将所有数据存储在此文件中。
    milvus_client = MilvusClient(uri="./" + productid + ".db")

    # 创建集合
    collection_name = deviceid

    #检查集合是否已经存在，如果存在则将其删除
    if milvus_client.has_collection(collection_name):
        milvus_client.drop_collection(collection_name)

    # 使用指定的参数创建一个新集合。
    # 如果我们不指定任何字段信息，Milvus将自动为主键创建一个默认的id字段，并创建一个向量字段来存储向量数据。
    # 保留的JSON字段用于存储未在schema里定义的标量数据
    milvus_client.create_collection(
        collection_name=collection_name,
        dimension=embedding_dim,
        metric_type="IP",  # Inner product distance
        consistency_level="Strong",  # Strong consistency level
    )

    # 插入数据
    # 逐条取出文本数据，创建嵌入，然后将数据插入Milvus。
    # 这里有一个新的字段“text”，它是集合schema中的非定义字段，会自动添加到保留的JSON动态字段中。
    data = []
    doc_embeddings = embedding_model.encode_documents(text_lines)
    for i, line in enumerate(tqdm(text_lines, desc="Creating embeddings")):
        data.append({"id": i, "vector": doc_embeddings[i], "text": line})
    milvus_client.insert(collection_name=collection_name, data=data)

def query(params):

    # 构建RAG
    # 检索查询数据
    # 让我们指定一个关于Milvus的常见问题。
    question = params['question']
    productid = params['productid']
    deviceid = params['deviceid']
    api_key = params['api_key']

    embedding_model = milvus_model.DefaultEmbeddingFunction()

    milvus_client = MilvusClient(uri="./" + productid + ".db")

    collection_name = deviceid


    # 在集合中搜索问题并检索语义top-3匹配项。
    search_res = milvus_client.search(
        collection_name=collection_name,
        data=embedding_model.encode_queries(
            [question]
        ),  # Convert the question to an embedding vector
        limit=3,  # Return top 3 results
        search_params={"metric_type": "IP", "params": {}},  # Inner product distance
        output_fields=["text"],  # Return the text field
    )

    # 我们来看一下query的搜索结果
    retrieved_lines_with_distances = [
        (res["entity"]["text"], res["distance"]) for res in search_res[0]
    ]
    # print(json.dumps(retrieved_lines_with_distances, indent=4))
    [
        [
            " Where does Milvus store data?\n\nMilvus deals with two types of data, inserted data and metadata. \n\nInserted data, including vector data, scalar data, and collection-specific schema, are stored in persistent storage as incremental log. Milvus supports multiple object storage backends, including [MinIO](https://min.io/), [AWS S3](https://aws.amazon.com/s3/?nc1=h_ls), [Google Cloud Storage](https://cloud.google.com/storage?hl=en#object-storage-for-companies-of-all-sizes) (GCS), [Azure Blob Storage](https://azure.microsoft.com/en-us/products/storage/blobs), [Alibaba Cloud OSS](https://www.alibabacloud.com/product/object-storage-service), and [Tencent Cloud Object Storage](https://www.tencentcloud.com/products/cos) (COS).\n\nMetadata are generated within Milvus. Each Milvus module has its own metadata that are stored in etcd.\n\n###",
            0.6572665572166443
        ],
        [
            "How does Milvus flush data?\n\nMilvus returns success when inserted data are loaded to the message queue. However, the data are not yet flushed to the disk. Then Milvus' data node writes the data in the message queue to persistent storage as incremental logs. If `flush()` is called, the data node is forced to write all data in the message queue to persistent storage immediately.\n\n###",
            0.6312146186828613
        ],
        [
            "How does Milvus handle vector data types and precision?\n\nMilvus supports Binary, Float32, Float16, and BFloat16 vector types.\n\n- Binary vectors: Store binary data as sequences of 0s and 1s, used in image processing and information retrieval.\n- Float32 vectors: Default storage with a precision of about 7 decimal digits. Even Float64 values are stored with Float32 precision, leading to potential precision loss upon retrieval.\n- Float16 and BFloat16 vectors: Offer reduced precision and memory usage. Float16 is suitable for applications with limited bandwidth and storage, while BFloat16 balances range and efficiency, commonly used in deep learning to reduce computational requirements without significantly impacting accuracy.\n\n###",
            0.6115777492523193
        ]
    ]

    # 使用LLM获取RAG响应
    # 将检索到的文档转换为字符串格式。
    context = "\n".join(
        [line_with_distance[0] for line_with_distance in retrieved_lines_with_distances]
    )

    # 为LLM定义系统和用户提示。这个提示是由从Milvus检索到的文档组装而成的。
    SYSTEM_PROMPT = """
    Human: You are an AI assistant. You are able to find answers to the questions from the contextual passage snippets provided.
    """

    USER_PROMPT = f"""
    Use the following pieces of information enclosed in <context> tags to provide an answer to the question enclosed in <question> tags.
    <context>
    {context}
    </context>
    <question>
    {question}
    </question>
    """

    # 准备LLM和embedding模型
    # 使用DeepSeek提供的deepseek-chat模型根据提示生成响应。
    try:
        client = OpenAI(
            # 若没有配置环境变量，请用百炼API Key将下行替换为：api_key="sk-xxx",
            api_key=api_key,
            base_url="https://dashscope.aliyuncs.com/compatible-mode/v1",
        )

        response = client.chat.completions.create(
        model="qwen-max",
        messages=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": USER_PROMPT},
            ],
        )
        print(response.choices[0].message.content)

    except Exception as e:
        print(f"错误信息：{e}")
        print("请参考文档：https://help.aliyun.com/zh/model-studio/developer-reference/error-code")

def main(argv):
    params = json.loads(base64.b64decode(argv).decode("utf-8"))
    action = params['action']
    match action:
        case "create_device":
            create_device(params)
        case "query":
            query(params)

def exit():
    os._exit(0)

if __name__ == "__main__":
    main(sys.argv[1])
