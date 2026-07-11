"""dgiot_ontology L3 深度学习推理服务 (Python + ONNX Runtime)"""
import time, os, json, logging
import numpy as np
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import onnxruntime as ort

app = FastAPI(title="DG-IoT Ontology L3 Inference")
logger = logging.getLogger("dgiot_ontology_l3")

MODEL_DIR = os.environ.get("ONNX_MODEL_DIR", "./models")
loaded_models = {}

def load_model(model_name: str):
    if model_name not in loaded_models:
        path = os.path.join(MODEL_DIR, model_name)
        if not os.path.exists(path):
            raise FileNotFoundError(f"ONNX model not found: {path}")
        loaded_models[model_name] = ort.InferenceSession(path)
        logger.info(f"Loaded: {model_name}")
    return loaded_models[model_name]

class SensorData(BaseModel):
    device_id: str
    model_name: str = "fcvae_gnn_v3.onnx"
    features: list[float]  # [vibration_rms, vibration_fft_1024(可选), temp, pressure, flow]
    threshold: float = 0.85

@app.post("/api/inference/anomaly")
async def detect_anomaly(data: SensorData):
    start = time.perf_counter()
    try:
        session = load_model(data.model_name)
        features = np.array(data.features, dtype=np.float32).reshape(1, -1)
        output = session.run(None, {"input": features})
        score = float(output[0][0])
        inference_ms = (time.perf_counter() - start) * 1000

        return {
            "device_id": data.device_id,
            "anomaly_score": round(score, 4),
            "is_anomaly": score > data.threshold,
            "confidence": round(min(score * 100, 99.0), 1),
            "inference_ms": round(inference_ms, 1),
            "model": data.model_name
        }
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

class TrendRequest(BaseModel):
    device_id: str
    history: list[list[float]]  # [[t1_values], [t2_values], ...]
    horizon: int = 30  # 未来30步预测

@app.post("/api/inference/trend")
async def predict_trend(data: TrendRequest):
    start = time.perf_counter()
    history = np.array(data.history, dtype=np.float32).reshape(1, len(data.history), -1)
    session = load_model("lstm_trend_v2.onnx")
    output = session.run(None, {"input": history})
    predictions = output[0][0].tolist()
    inference_ms = (time.perf_counter() - start) * 1000

    return {
        "device_id": data.device_id,
        "horizon_steps": data.horizon,
        "predictions": predictions[:data.horizon],
        "trend": "degrading" if predictions[-1] > predictions[0] * 1.1 else "stable",
        "inference_ms": round(inference_ms, 1)
    }

@app.get("/api/inference/health")
async def health():
    return {
        "status": "healthy",
        "loaded_models": list(loaded_models.keys()),
        "model_dir": MODEL_DIR
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8082)
